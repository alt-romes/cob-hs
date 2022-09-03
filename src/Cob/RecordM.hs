{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob.RecordM where

-- TODO: Make Async versions of functions that receive a callback or return an async token?

-- TODO: Make rmGetInstance that searches directly for reference. Remove Ref as
-- an instance of Record to find them or make body of search conditional on
-- whether it's a ref or not..


-- * Deprecation in progres...

--- RecordM

-- | RecordM writes the added instances thorought the Cob computation.
-- This allows it to undo (delete) all added instances in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runRecordMTests', all added instances will be deleted
type instance CobWriter 'RecordM = Ref ()

-- * Definitions and Records

newtype ParseError = ParseError String deriving Show
instance Exception ParseError

-- | Like 'rmDefinitionSearch', but returns a lazy list of all
-- records, rather than a range.
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


-- | Search a @RecordM@ 'Definition' given a 'RecordMQuery', exactly the same as
-- 'rmDefinitionSearch', but ignore the records references ('Ref').
-- Instead, this function returns only a list of the records ('Record') found.
--
-- See also 'rmDefinitionSearch'
-- rmDefinitionSearch_ :: forall a. Record a => Query a -> Cob IO [a]
-- rmDefinitionSearch_ q = map snd <$> rmDefinitionSearch q
-- {-# INLINE rmDefinitionSearch_ #-}

-- ROMES:TODO: Don't ignore sorting!

-- ROMES:TODO: Search
-- rmDefinitionSum :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> Cob m (Count a)
-- rmDefinitionSum rmQuery = do
-- {-# INLINABLE rmDefinitionSum #-}


-- | Update an instance with an id and return the updated record.
-- An error will be thrown if no record is successfully updated.
-- rmUpdateInstance :: forall a. Record a => Ref a -> (a -> a) -> Cob IO a
-- rmUpdateInstance ref f = rmUpdateInstances_ (byId ref) f ??? throwError ("Updating instance " <> show ref <> " was not successful!")
-- {-# INLINE rmUpdateInstance #-}

-- | Update in @RecordM@ all instances matching a query given a function that
-- transforms records, and return the list of updated records Warning: This
-- update is not atomic (yet? :TODO:). This means that in between applying the
-- function to the value and sending the update, it might have been updated
-- somewhere else, and this update will overwrite the record with the modified
-- previous record. This also means that if more than one record matches the
-- query, the list of records will NOT be updated in batch -- on an error
-- message, if the error occured in between updating one of the records in the
-- middle of the list, the first updates might have occurred despite the error
--
-- If the need ever arises, an atomic implementation could possibly set the
-- correct version:x on the query and check for number of successful updates,
-- though this doesn't solve the list batch update
--
-- Another note: The query will limit the amount of instances fetched. That
-- means more instances could match the query but aren't currently fetched and
-- won't be updated.
-- rmUpdateInstances :: forall a. Record a => Query a -> (a -> a) -> Cob IO [(Ref a, a)]
-- rmUpdateInstances q f = rmUpdateInstancesM q (return <$> f)
-- {-# INLINE rmUpdateInstances #-}

-- | The same as 'rmUpdateInstance', but the function to update the record returns a 'CobT'
-- rmUpdateInstancesM :: forall a. Record a => Query a -> (a -> Cob IO a) -> Cob IO [(Ref a, a)]
-- rmUpdateInstancesM rmQuery updateRecord = do
--     records <- rmDefinitionSearch rmQuery
--     session <- ask
--     forM records $ \(Ref ref, rec) -> do
--         updatedRecord <- updateRecord rec
--         let request = setRequestBodyJSON
--                       (object
--                           [ "type"      .= definition @a
--                           , "condition" .= ("id:" <> show ref)
--                           , "values"    .= updatedRecord ])
--                       (cobDefaultRequest session)
--                           { method = "PUT"
--                           , path   = "/recordm/recordm/instances/integration" }
--         _ <- httpValidJSON @Value request
--         return (Ref ref, updatedRecord)

-- | The same as 'rmUpdateInstance' but discard the @'Ref' a@ from @('Ref' a, a)@ from the result
-- rmUpdateInstances_ :: forall a. Record a => Query a -> (a -> a) -> Cob IO [a]
-- rmUpdateInstances_ q = fmap (map snd) . rmUpdateInstances q
-- {-# INLINE rmUpdateInstances_ #-}

-- | The same as 'rmUpdateInstancesWithMakeQueryM' but the update record function does not return the value within a monad @m@
-- rmUpdateInstancesWithMakeQuery :: forall a b. (Record a, Record b) => Query a -> (a -> Query b) -> (b -> b) -> Cob IO [(Ref b, b)]
-- rmUpdateInstancesWithMakeQuery q f g = rmUpdateInstancesWithMakeQueryM q f (return <$> g)
-- {-# INLINE rmUpdateInstancesWithMakeQuery #-}

-- | Run a @'RecordMQuery' q@ and transform all resulting records (@'Record' a@)
-- into new queries (@'RecordMQuery' r@). Finally, update all resulting records
-- (@'Record' b@) with the third argument, the function (@b -> 'CobT' m b@).
-- rmUpdateInstancesWithMakeQueryM :: forall a b. (Record a, Record b) => Query a -> (a -> Query b) -> (b -> Cob IO b) -> Cob IO [(Ref b, b)]
-- rmUpdateInstancesWithMakeQueryM rmQuery getRef updateRecord = rmDefinitionSearch_ rmQuery >>= fmap join . mapM (flip rmUpdateInstancesM updateRecord . getRef)
