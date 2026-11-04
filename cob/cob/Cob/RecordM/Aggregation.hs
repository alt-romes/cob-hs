{-# LANGUAGE OverloadedStrings #-}
{-|

TODO: Figure out .raw, and _exists_ bits...

Typed Elasticsearch aggregations.

An @'Agg' a@ describes both the @aggs@ JSON sent to Elasticsearch and how to
parse the response into an @a@. Aggregations are combined with the
'Applicative' interface, which generates the aggregation names, so results
are never looked up by string.

=== Example

@
type PerItem = Map Text Double

stock :: Agg (Map Text PerItem, Map Text PerItem, PerItem)
stock = (,,) \<$\> terms "warehouse_destiny.raw" 100 perItem
             \<*\> terms "warehouse_origin.raw"  100 perItem
             \<*\> filtered "client_type:Person AND NOT order_state:Refund AND NOT _exists_:delivery_date" perItem
  where perItem = terms "stock_item.raw" 1000 (sumField "qt")

(in, out, committed) <- aggregate \@Movement "_exists_:stock_item" stock
@
 -}
module Cob.RecordM.Aggregation
  ( Agg
    -- * Bucket aggregations
  , terms
  , filtered
  , docCount
    -- * Metric aggregations
  , sumField
  , avgField
  , minField
  , maxField
  , cardinality
    -- * Running
  , aggregate
  , aggregateById
    -- * Debug
  , agg_body
  , agg_parser
  ) where

import Control.Monad.RWS.CPS
import Data.Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Cob.Log
import Cob.RecordM.Definition (DefinitionId)
import Cob.RecordM.Query
import Cob.RecordM.Record
import Cob.RecordM.Servant (structuredSearchByName, structuredSearchById)
import Cob.Utils

-- | An aggregation producing an @a@.
--
-- Draws fresh names from the counter, writes the named aggregations to send,
-- and returns a parser for the object holding their results.
newtype Agg a = Agg (RWS () [Pair] Int (Object -> Parser a))

instance Functor Agg where
  fmap f (Agg m) = Agg (fmap (fmap (fmap f)) m)

instance Applicative Agg where
  pure x = Agg (pure (\_ -> pure x))
  Agg f <*> Agg x = Agg (liftA2 (liftA2 (<*>)) f x)

runAgg :: Agg a -> ([Pair], Object -> Parser a)
runAgg (Agg m) = (ps, p)
  where (p, ps) = evalRWS m () 0

-- | A single aggregation with the given body and result parser, under a fresh name.
named :: Value -> (Value -> Parser a) -> Agg a
named body p = Agg $ do
  k <- state (\n -> (fromString ('a' : show n), n + 1))
  tell [k .= body]
  pure (\o -> o .: k >>= p)

-- | Nest an aggregation under a bucket aggregation's body.
nested :: [Pair] -> Agg a -> (Value, Object -> Parser a)
nested body sub = (object (body <> aggs), p)
  where
    (ps, p) = runAgg sub
    aggs = ["aggs" .= object ps | not (null ps)]

queryString :: Query x -> Value
queryString q = object ["query_string" .= object ["query" .= _q q, "default_operator" .= ("AND" :: Text)]]

--------------------------------------------------------------------------------
-- Bucket aggregations

-- | Group by the values of a field, keeping at most @size@ buckets (those with
-- the most documents), and run the sub-aggregation in each bucket.
--
-- Note: Elasticsearch defaults @size@ to 10, silently dropping the remaining buckets.
terms :: Text -> Int -> Agg a -> Agg (Map Text a)
terms field size sub = named body $ withObject "terms" $ \o -> do
  buckets <- o .: "buckets"
  M.fromList <$> mapM (withObject "bucket" $ \b -> (,) <$> b .: "key" <*> p b) buckets
  where
    (body, p) = nested ["terms" .= object ["field" .= field, "size" .= size]] sub

-- | Run the sub-aggregation only over the documents matching the query.
filtered :: Query x -> Agg a -> Agg a
filtered q sub = named body (withObject "filter" p)
  where
    (body, p) = nested ["filter" .= queryString q] sub

-- | The number of documents in the enclosing bucket.
--
-- Only valid inside 'terms' or 'filtered'.
docCount :: Agg Int
docCount = Agg (pure (.: "doc_count"))

--------------------------------------------------------------------------------
-- Metric aggregations

metric :: FromJSON a => Text -> Text -> Agg a
metric kind field = named (object [fromText kind .= object ["field" .= field]]) (withObject "metric" (.: "value"))

sumField :: Text -> Agg Double
sumField = metric "sum"

-- | 'Nothing' when there are no values to aggregate.
avgField, minField, maxField :: Text -> Agg (Maybe Double)
avgField = metric "avg"
minField = metric "min"
maxField = metric "max"

-- | Approximate number of distinct values.
cardinality :: Text -> Agg Int
cardinality = metric "cardinality"

--------------------------------------------------------------------------------
-- Running

-- | Run an aggregation over the instances of a definition matching the query.
aggregate :: forall a b m. (MonadCob m, Record a) => Query a -> Agg b -> m b
aggregate q agg = do
  logInfo $ "Aggregating definition " <> toLogStr (definition @a) <> " with " <> toLogStr (_q q)
  performReq (structuredSearchByName (definition @a) (agg_body q agg)) >>= parse_aggregations agg

-- | Like 'aggregate', but for a definition given by its id.
aggregateById :: MonadCob m => DefinitionId -> Query x -> Agg b -> m b
aggregateById defId q agg = do
  logInfo $ "Aggregating definition " <> toLogStr (show defId) <> " with " <> toLogStr (_q q)
  performReq (structuredSearchById defId (agg_body q agg)) >>= parse_aggregations agg

parse_aggregations :: MonadIO m => Agg b -> Value -> m b
parse_aggregations agg = parseOrThrowIO $ withObject "aggregate" $ \o -> do
  aggs <- o .:? "aggregations" .!= Object mempty
  withObject "aggregations" (agg_parser agg) (untyped aggs)
  where
    -- Strip the @type#@ prefix RecordM adds to aggregation names (@typed_keys@
    -- defaults to true, and @typed_keys=false@ isn't honoured inside @filter@).
    untyped :: Value -> Value
    untyped (Object o) = Object (KM.fromList [ (strip k, untyped v) | (k, v) <- KM.toList o ])
      where strip = fromText . T.takeWhileEnd (/= '#') . toText
    untyped (Array a) = Array (fmap untyped a)
    untyped v = v

-- | The Elasticsearch search body for an aggregation over the query.
agg_body :: Query x -> Agg a -> Value
agg_body q agg = object ["size" .= (0 :: Int), "query" .= queryString q, "aggs" .= object (fst (runAgg agg))]

-- | Parse the @aggregations@ object of the response.
agg_parser :: Agg a -> Object -> Parser a
agg_parser = snd . runAgg
