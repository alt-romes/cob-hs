{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
module Cob.RecordM
  ( module Cob.RecordM
  -- ** Re-exports
  , MonadCob
  ) where

import Data.Aeson

import Control.Exception
import Cob.Exception

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Streamly.Data.Stream as Streamly
import qualified Streamly.Data.Stream.Prelude as Streamly

import qualified Servant.Client
import qualified Servant.Client.Streaming

import Cob.RecordM.Servant as Servant
import Cob.RecordM.Record
import Cob.RecordM.Query
import Cob.RecordM.Definition
import Cob.Utils
import Cob.Session
import Cob.Ref
import Data.Maybe

-- TODO: Nested fields! # Creating an instance with duplicate fields https://learning.cultofbits.com/docs/cob-platform/developers/recordm-integration-resource/#creating-an-instance-with-duplicate-fields

-- TODO: Make Async versions of functions that receive a callback or return an async token???

-- TODO: Make rmGetInstance that searches directly for reference. Remove Ref as
-- an instance of Record to find them or make body of search conditional on
-- whether it's a ref or not..


-- ROMES:TODO: Add custom errors and document which functions throw which errors

-- ROMES:TODO: Don't ignore sorting!

-- ROMES:TODO: I removed `encode`, did I break encoding or will servant take care of it?

--------------------------------------------------------------------------------
-- * Querying definitions
--------------------------------------------------------------------------------

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
definitionSearch :: forall a m. MonadCob m => Record a => Query a -> m [(Ref a, a)]
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

-- | Stream a definition!
streamDefinitionSearch :: forall a b m. MonadCob m => Record a => Query a -> (Streamly.Stream IO (Ref a, a) -> IO b) -> m b
streamDefinitionSearch rmQuery f = do
  CobSession{clientEnv} <- ask
  let req = streamSearchByName (definition @a) (Just (_q rmQuery)) Nothing
  liftIO $ Servant.Client.Streaming.withClientM req clientEnv (\case
    Left e -> throwIO e
    Right stream ->
      let
        parseRefA = withObject "streaming_search" $ \o -> do
          src <- o .: "_source"
          r   <- parseJSON @(Ref a) src
          a   <- parseJSON @a src
          pure (r, a)
      in
        f (Streamly.mapM (parseOrThrowIO parseRefA) stream))


-- | Get an instance by id and fail if the instance isn't found
--
-- TODO: Use /recordm/instances/{id} instead of definitions/search?
-- Difficulty of ^ is parsing
-- Otherwise:
-- rbody <- performReq (getInstance (fromInteger ref) Nothing) `catchError` throwError ("Couldn't find instance with id " <> show ref)
getInstance :: forall a m. MonadCob m => Record a => Ref a -> m a
getInstance ref = do
  definitionSearch (defaultQuery { _q = "id:" <> show ref, _size = 1 }) >>= \case
    [] -> liftIO $ fail ("Couldn't find instance with id " <> show ref)
    (_,x):_ -> pure x
{-# INLINE getInstance #-}

-- | Count the number of records matching a query in a definition
definitionCount :: forall a m. MonadCob m => Record a => Query a -> m Int
definitionCount rmQuery = do
    rbody <- performReq $ searchByName (definition @a) (Just (_q rmQuery)) (Just 0) (Just 0) Nothing
    parseOrThrowIO (withObject "definition_count" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody


--------------------------------------------------------------------------------
-- * Creating instances
--------------------------------------------------------------------------------

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
addInstance :: forall a m. MonadCob m => Record a => a -> m (Ref a)
addInstance = addInstanceWith False
{-# INLINE addInstance #-}

-- | Same as 'rmAddInstance' but only continue when added record is searchable.
--
-- The difference is the query parameter waitForSearchAvailability=true
-- Related to documentation on POST /recordm/instances/integration.
addInstanceSync :: forall a m. MonadCob m => Record a => a -> m (Ref a)
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
addInstanceWith :: forall a m. MonadCob m => Record a => Bool -> a -> m (Ref a)
addInstanceWith waitForSearchAvailability record = do
  -- Last I checked (Nov. 2023), instance creation doesn't return a version
  -- alongside the newly created ID, so we give it the initial version (which is 0).
  Ref no_version refid <- performReq $ Servant.addInstance (AddSpec (definition @a) record waitForSearchAvailability)
  assert (isNothing no_version) $
    return (Ref (Just 0) refid)


--------------------------------------------------------------------------------
-- * Deleting instances
--------------------------------------------------------------------------------

-- | Delete an instance by id, ignoring if it has any references
--
-- See /recordm/instances/{id}
--
-- We don't do version checking! This will really delete the instance regardless of whether you were the last to update it.
deleteInstance :: forall a m. MonadCob m => Ref a -> m ()
deleteInstance (Ref _ ref) = do
  _ <- performReq $ Servant.deleteInstance ref (Just True)
  pure ()

-- | Like 'rmDefinitionSearch', but returns a lazy list of all
-- records, rather than a range.
--
-- ROMES:TODO: I think this is superseded by streamDefinition, right?
--
-- The search starts from the `from` field in the query, but ignores the `size` field
--
-- The return result is a list of all records that will only be fetched as needed;
--
-- Example
-- ======
--
-- @
-- 
-- @
--
--
-- The records in the list will only be fetched as needed, but operations such
-- as `!!` will unfortunately require that all items until the requested item
-- be forced (dev note: because we can't assume the number of records
-- available, we must fetch batches as needed until one turns to be empty: one
-- way or another we'll have to join lists of lists... so we can't get *just the requested item*, but rather all up until the requested one.
-- see more notes in the source code)
--
-- You should be careful using this abstraction, e.g. a function such as
-- `length` will force all values to be fetched. Furthermore, errors won't be
-- propagated as CobErrors, but rather as IOExceptions
--
-- Bottom line: this is a very convenient abstraction; use it wisely.
-- rmLazyDefinitionSearch :: forall a. Record a => Query a -> Cob IO [(Ref a, a)]
-- rmLazyDefinitionSearch rmQuery = do
--     ask >>= liftIO . go (_from rmQuery)

    --
    -- This commented only kind of works on the assumption that the number of records
    -- doesn't change, which is false; I couldn't find a way to think about
    -- this so I settled for the "take as you need" version
    --
    -- session <- ask
    -- Count count <- rmDefinitionCount @a @q rmQuery
    -- let records = map (unsafePerformIO . searchRange session) [0,batchSize..fromInteger count-1]
    -- return $
    --     flip map [0..fromInteger count-1] $ \x ->
    --         case splitAt (x `mod` batchSize) (records !! (x `div` batchSize)) of
    --           (_, i:_) -> i
    --           _ -> error "bad"



    -- where 
    --     go :: Int -> CobSession -> IO [(Ref a, a)]
    --     go from session = unsafeInterleaveIO $ do
    --       searchRange session from >>= \case
    --         [] -> return []
    --         vs
    --           | length vs < batchSize -- Don't fetch if this batch was smaller than expected
    --           -> return vs
    --           | otherwise
    --           ->
    --             (vs <>) <$> go (from + batchSize) session

    --     batchSize = 2

    --     searchRange :: CobSession -> Int -> IO [(Ref a, a)] 
    --     searchRange session from = do
    --         -- putStrLn ("Fetching things from " <> show from)
    --         let request = (cobDefaultRequest session)
    --                           { path = "/recordm/recordm/definitions/search/name/" <> fromString (encode $ definition @a)
    --                           , queryString = renderRMQuery @a rmQuery {_from = from, _size = batchSize} }
    --         rbody <- httpValidJSON' request userError
    --         hits  <- getResponseHitsHits rbody userError          -- Get hits.hits from response body
    --         let hitsSources = mapMaybe (parseMaybe (withObject "wO" (.: "_source"))) hits    -- Get _source from each hit
    --         let ids = mapMaybe (parseMaybe parseJSON) hitsSources -- Get id from each _source
    --         records <- (either (throwError . userError) return . parseEither parseJSON . Array . V.fromList) hitsSources  -- Parse record from each _source
    --         return (zip ids records)                              -- Return list of (id, record)


--------------------------------------------------------------------------------
-- * Updating instances
--------------------------------------------------------------------------------

-- | Update all instances matching a query and return the updated records.
--
-- This update is atomic! TODO: Should we retry instead of failing?
-- If the record is changed while the function is running, the record won't be updated and (ideally 'OutdatedVersion', but really) 'UnknownUpdateError' will be thrown
--
-- This function might throw the following 'CobException's:
--  'UnknownUpdateError'
--
-- The resulting list of updated values doesn't necessarily reflect the order of the query results, I think
-- TODO: Consider not returning updated records?
updateInstances :: forall a m. MonadCob m => Record a => Query a -> (a -> a) -> m [(Ref a, a)]
updateInstances query f = do
  CobSession{clientEnv} <- ask
  streamDefinitionSearch query $ Streamly.toList . Streamly.parEval id . Streamly.mapM (updateInstance' clientEnv)
    where
      updateInstance' :: Servant.Client.ClientEnv -> (Ref a, a) -> IO (Ref a, a)
      updateInstance' session (Ref version ref, a) =
        let
            updatedRecord = f a
            req = Servant.updateInstances (Servant.UpdateSpec (definition @a) ("recordmInstanceId:" <> show ref <> maybe (error "updateInstances: All records streamed from a definition should have a version together with the instance id") (('@':) . show) version) updatedRecord)
         in
            Servant.Client.runClientM req session >>= \case
              Left e -> throwIO e
              Right u
                -- TODO: We could have more precise errors using the other API, and possibly even automatically retry until successful.
                | errorOS u > 0 -> throwIO (UnknownUpdateError $ Ref version ref)
                | otherwise     -> pure (Ref (fmap (+1) version) ref, updatedRecord)


--------------------------------------------------------------------------------
-- * Creating Definitions
--------------------------------------------------------------------------------

-- | Create a RecordM 'Definition', typically where 'Definition' is created
-- using 'fromDSL' and 'DefinitionQ'.
newDefinition :: forall m. MonadCob m => Definition -> m ()
newDefinition def = do
  _ <- performReq $ Servant.newDefinition def
  pure ()

