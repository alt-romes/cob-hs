{-# LANGUAGE OverloadedStrings #-}
module Cob.RecordM where

import Data.Aeson

import Control.Exception

import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Prelude as Streamly

import qualified Servant.Client.Streaming

import Cob.RecordM.Servant as Servant
import Cob.RecordM.Record
import Cob.RecordM.Query
import Cob.Utils
import Cob.Session
import Cob.Ref

-- ROMES:TODO: Add custom errors and document which functions throw which errors

-- ROMES:TODO: Don't ignore sorting!

-- ROMES:TODO: I removed `encode`, did I break encoding or will servant take care of it?

-- * Querying definitions

-- | Search a @RecordM@ 'Definition' given a 'Query, and return a list of references ('Ref') of a record and the corresponding records ('Record').
--
-- The 'Record' type to search for is inferred from the usage of the return element.
-- When the information available isn't enough to correctly infer the search 'Record' type, an explicit type can be used to help the compiler.
--
-- ==== __Example__
--
-- @
-- dogs <- rmDefinitionSearch @DogsRecord "bobby"
--
-- rmDefinitionSearch (defaultRMQuery { _q = "id:123"
--                                    , _from = 1
--                                    , _size = 21
--                                    , _sort = Just \"id\"
--                                    , _ascending = Just True })
--
-- rmDefinitionSearch (Ref 11223)
--
-- @
--
-- @Note@: the @OverloadedStrings@ and @ScopedTypeVariables@ language extensions must be enabled for the example above
definitionSearch :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => Query a -> m [(Ref a, a)]
definitionSearch rmQuery = do
    rbody <- performReq $ searchByName (definition @a) (Just (_q rmQuery)) (Just (_from rmQuery)) (Just (_size rmQuery)) Nothing
    let
      parser = withObject "definition_search" $ \o -> do
        o'   <- o  .: "hits"
        hits <- o' .: "hits"
        forM hits $ \h -> do
          src <- h .: "_source"
          r <- parseJSON @(Ref a) src
          a <- parseJSON @a src
          pure (r, a)
    parseOrThrowIO parser rbody
    -- hits  <- getResponseHitsHits rbody id                 -- Get hits.hits from response body 
    -- let hitsSources = mapMaybe (parseMaybe (withObject "wO" (.: "_source"))) hits    -- Get _source from each hit
    -- let ids = mapMaybe (parseMaybe parseJSON) hitsSources -- Get id from each _source
    -- records <- (either throwError return . parseEither parseJSON . Array . V.fromList) hitsSources  -- Parse record from each _source
    -- return (zip ids records)                              -- Return list of (id, record)
      -- where
        -- Gets hits.hits from response body, parsed as an array of JSON values
        -- getResponseHitsHits :: Value -> (String -> e) -> m [Value]
        -- getResponseHitsHits responseBody mkError = maybe
        --         (throwError . mkError $ "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        --         (either (throwError . mkError) return . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        --         (parseMaybe (withObject "wO" ((.: "hits") >=> (.: "hits"))) responseBody) -- Find hits.hits in response body


-- | Stream a definition!
streamDefinitionSearch :: forall a b m. (MonadReader CobSession m, MonadIO m) => Record a => Query a -> Fold.Fold IO (Ref a, a) b -> m b
streamDefinitionSearch rmQuery f = do
  CobSession session <- ask
  let req = streamSearchByName (definition @a) (Just (_q rmQuery)) Nothing
  liftIO $ Servant.Client.Streaming.withClientM req session (\case
    Left e -> throwIO e
    Right stream ->
      let
        parseRefA = withObject "streaming_search" $ \o -> do
          src <- o .: "_source"
          r   <- parseJSON @(Ref a) src
          a   <- parseJSON @a src
          pure (r, a)
      in
        Streamly.fold f (Streamly.mapM (parseOrThrowIO parseRefA) stream))


-- | Get an instance by id and fail if the instance isn't found
--
-- TODO: Use /recordm/instances/{id} instead of definitions/search?
-- Difficulty of ^ is parsing
-- Otherwise:
-- rbody <- performReq (getInstance (fromInteger ref) Nothing) `catchError` throwError ("Couldn't find instance with id " <> show ref)
getInstance :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => Ref a -> m a
getInstance ref = do
  definitionSearch (defaultQuery { _q = "id:" <> show ref, _size = 1 }) >>= \case
    [] -> liftIO $ fail ("Couldn't find instance with id " <> show ref)
    (_,x):_ -> pure x
{-# INLINE getInstance #-}

-- | Count the number of records matching a query in a definition
definitionCount :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => Query a -> m Int
definitionCount rmQuery = do
    rbody <- performReq $ searchByName (definition @a) (Just (_q rmQuery)) (Just 0) (Just 0) Nothing
    parseOrThrowIO (withObject "definition_count" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody

-- * Creating instances

-- | Add to @RecordM@ a new instance given a 'Record', and return the 'Ref' of the newly created instance.
--
-- Note: By default RecordM answers before the added record being searchable.
-- If you want to add an instance but only continue when it's searchable, see
-- 'rmAddInstanceSync'
--
-- ==== __Example__
--
-- @
-- addDog :: Text -> Text -> Cob (Ref DogsRecord)
-- addDog name ownerName = do
--      rmAddInstance (DogsRecord name ownerName 0)
-- @
addInstance :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => a -> m (Ref a)
addInstance = addInstanceWith False
{-# INLINE addInstance #-}

-- Same as 'rmAddInstance' but only continue when added record is searchable.
--
-- The difference is the query parameter waitForSearchAvailability=true
-- Related to documentation on POST /recordm/instances/integration.
addInstanceSync :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => a -> m (Ref a)
addInstanceSync = addInstanceWith True
{-# INLINE addInstanceSync #-}

-- | Add an instance to RecordM and set waitForSearchAvailability to the bool passed in the first paramater.
--
-- This is the internal method used by 'rmAddInstance' and 'rmAddInstanceSync
--
-- Example
-- @
-- ref <- rmAddInstanceWith True (ServersRecord ...)
-- -- Only because waitForSearchAvailability is true ^, will the update be able to
-- -- find the added instance and update it
-- rmUpdateInstance ref (property .~ newValue)
-- @
addInstanceWith :: forall a m. (MonadReader CobSession m, MonadIO m) => Record a => Bool -> a -> m (Ref a)
addInstanceWith waitForSearchAvailability record =
  performReq $ Servant.addInstance (AddSpec (definition @a) record waitForSearchAvailability)

-- * Deleting instances

-- | Delete an instance by id, ignoring if it has any references
--
-- See /recordm/instances/{id}
deleteInstance :: forall a m. (MonadReader CobSession m, MonadIO m) => Ref a -> m ()
deleteInstance (Ref ref) = do
  _ <- performReq $ Servant.deleteInstance ref (Just True)
  pure ()

