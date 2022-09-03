module Cob.RecordM.Interpreter where

import Cob.RecordM.Ref
import Cob.RecordM.Record
import Cob.RecordM.Query

-- ROMES:TODO: Don't ignore sorting!

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
rmDefinitionSearch :: forall a. Record a => Query a -> Cob IO [(Ref a, a)]
rmDefinitionSearch rmQuery = do
    rbody <- performReq $ searchByName (encode (definition @a)) (Just (_q rmQuery)) (Just (_from rmQuery)) (Just (_size rmQuery)) Nothing
    hits  <- getResponseHitsHits rbody id                 -- Get hits.hits from response body 
    let hitsSources = mapMaybe (parseMaybe (withObject "wO" (.: "_source"))) hits    -- Get _source from each hit
    let ids = mapMaybe (parseMaybe parseJSON) hitsSources -- Get id from each _source
    records <- (either throwError return . parseEither parseJSON . Array . V.fromList) hitsSources  -- Parse record from each _source
    return (zip ids records)                              -- Return list of (id, record)
      where
        -- | Gets hits.hits from response body, parsed as an array of JSON values
        getResponseHitsHits :: Value -> (String -> e) -> m [Value]
        getResponseHitsHits responseBody mkError = maybe
                (throwError . mkError $ "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
                (either (throwError . mkError) return . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
                (parseMaybe (withObject "wO" ((.: "hits") >=> (.: "hits"))) responseBody) -- Find hits.hits in response body


-- | Stream a definition!
rmStreamDefinitionSearch :: forall a b. Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> Cob IO b
rmStreamDefinitionSearch rmQuery f = do
  CobSession session <- ask
  let req = streamSearchByName (encode (definition @a)) (Just (_q rmQuery)) Nothing
  liftIO $ withClientM req session (\case
    Left e -> throwIO e
    Right stream ->
      let 
        parseRefA = withObject "streaming_search" $ \o -> do
          src <- o .: "_source"
          r   <- parseJSON @(Ref a) src
          a   <- parseJSON @a src
          pure (r, a)
      in stream
      & Streamly.mapM (either (throwIO . SomeException . ParseError) pure . parseEither parseRefA)
      & f
                                   )


-- | Get an instance by id and fail if the instance isn't found
--
-- TODO: Use /recordm/instances/{id} instead of definitions/search?
-- Difficulty of ^ is parsing
-- Otherwise:
-- rbody <- performReq (getInstance (fromInteger ref) Nothing) `catchError` throwError ("Couldn't find instance with id " <> show ref)
rmGetInstance :: forall a. Record a => Ref a -> Cob IO a
rmGetInstance ref = snd <$> (rmDefinitionSearch (defaultQ { _q = "id:" <> show ref, _size = 1 })) ??? throwError ("Couldn't find instance with id " <> show ref)
{-# INLINE rmGetInstance #-}

-- | Count the number of records matching a query in a definition
rmDefinitionCount :: forall a. Record a => Query a -> Cob IO Int
rmDefinitionCount rmQuery = do
    rbody <- performReq $ searchByName (encode (definition @a)) (Just (_q rmQuery)) (Just 0) (Just 0) Nothing
    count <- parseMaybe (withObject "wO" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody ?? throwError "Couldn't find hits.total.value when doing a definition count"
    return count

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
rmAddInstance :: forall a. Record a => a -> Cob IO (Ref a)
rmAddInstance = rmAddInstanceWith False
{-# INLINE rmAddInstance #-}

-- Same as 'rmAddInstance' but only continue when added record is searchable.
--
-- The difference is the query parameter waitForSearchAvailability=true
-- Related to documentation on POST /recordm/instances/integration.
rmAddInstanceSync :: forall a. Record a => a -> Cob IO (Ref a)
rmAddInstanceSync = rmAddInstanceWith True
{-# INLINE rmAddInstanceSync #-}

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
rmAddInstanceWith :: forall a. Record a => Bool -> a -> Cob IO (Ref a)
rmAddInstanceWith waitForSearchAvailability record = do
  ref <- performReq $ addInstance (AddSpec (definition @a) record waitForSearchAvailability)
  tell (singleton (Ref . ref_id $ ref), mempty)
  return ref

-- * Deleting instances

-- | Delete an instance by id, ignoring if it has any references
--
-- See /recordm/instances/{id}
rmDeleteInstance :: forall a. Ref a -> Cob IO ()
rmDeleteInstance (Ref ref) = do
  performReq $ deleteInstance (fromInteger ref) (Just True)
