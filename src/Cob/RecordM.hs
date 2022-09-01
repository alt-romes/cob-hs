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
module Cob.RecordM
  ( module Cob.RecordM
  , module Cob.RecordM.Ref ) where

-- TODO: Make Async versions of functions that receive a callback or return an async token

-- TODO: Instance Default with StandardRecordMQuery?

-- TODO: Make rmGetInstance that searches directly for reference. Remove Ref as
-- an instance of Record to find them or make body of search conditional on
-- whether it's a ref or not..

-- import System.IO.Unsafe -- This is used solely to generate the lazy list of records

import Data.Maybe (mapMaybe)
import Cob.RecordM.Servant

import qualified Data.Vector as V ( fromList   )

import Control.Monad              ((>=>))
import Control.Monad.Except       ( MonadError, throwError )
import Control.Monad.Writer       ( tell                    )

import Data.DList ( singleton )

import Data.Aeson.Types ( ToJSON, FromJSON, parseJSON, Value(..), withObject, (.:), parseEither, parseMaybe )

import Data.Text          ( Text       )
import Data.ByteString    ( ByteString )
import Data.String        ( fromString, IsString )
import Network.URI.Encode ( encode     )

import Cob.RecordM.Ref
import Cob

--- RecordM

-- | RecordM writes the added instances thorought the Cob computation.
-- This allows it to undo (delete) all added instances in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runRecordMTests', all added instances will be deleted
type instance CobWriter 'RecordM = Ref ()

--- Definitions and Records

-- | 'Definition' is a type synonym modelling a RecordM Definition
type Definition = String

-- | A 'Record' is an instance belonging to a RecordM 'Definition'
-- 
-- The definition name used depends on the type of the 'Record',
-- which will allow the compiler to automatically target the correct definition
-- when generating code to interact with RecordM
--
-- In the future, record types for each definition could be generated automatically
--
-- ==== __Example__
--
-- Given a RecordM definition called /Dogs/ with mandatory fields /Name/, /Owner Name/ and /Age/,
-- we could define the following type that instances Record:
--
-- @
-- data DogsRecord = DogsRecord Text Text Int
--
-- instance ToJSON DogsRecord where
--     toJSON (DogsRecord name ownerName age) = object
--         [ \"Name\"       .= name
--         , \"Owner name\" .= ownerName
--         , \"Age\"        .= age ]
-- instance FromJSON DogsRecord where
--     parseJSON = withObject "user record" $ \v -> do
--         [name] <- v .: "name"
--         [ownerName] <- v .: "owner_name"
--         [age] <- v .: "age"
--         return (DogsRecord name ownerName (read age))
--
-- instance Record DogsRecord where
--     definition = \"Dogs\"
-- @
class (ToJSON a, FromJSON a) => Record a where
    -- | Get the RecordM 'Definition' that has this type of 'Record'
    definition :: Definition


-- | A @RecordM@ query
data Query a = Query { _q         :: Text
                     , _from      :: Int
                     , _size      :: Int
                     , _sort      :: Maybe ByteString
                     , _ascending :: Maybe Bool
                     } deriving (Show)

instance IsString (Query a) where
  fromString txt = defaultRMQuery { _q = fromString txt }

-- | The default 'RecordMQuery'
-- @
-- defaultRMQuery = Query { _q         = "*"
--                        , _from      = 0
--                        , _size      = 5
--                        , _sort      = Nothing
--                        , _ascending = Nothing }
-- @
--
-- It is best used with the record update syntax to construct the desired query.
--
-- ==== __Example__
--
-- @
-- rmDefinitionSearch_ (defaultRMQuery { _q = "id:123*"
--                                     , _from = 1
--                                     , _size = 21
--                                     , _sort = Just \"id\"
--                                     , _ascending = Just True })
-- @
defaultRMQuery :: Query a
defaultRMQuery = Query { _q         = "*"
                       , _from      = 0
                       , _size      = 5
                       , _sort      = Nothing
                       , _ascending = Nothing
                       }

-- | 'Query' by 'Ref'
byId :: Ref a -> Query a
byId (Ref r) = defaultRMQuery { _q = "id:" <> fromString (show r) }

-- | 'Query' by 'String'
byStr :: String -> Query a
byStr t = defaultRMQuery { _q = fromString t }

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

-- TODO: Use streaming cob API + conduit-http sinks for streaming large amounts of data

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

-- | Get an instance by id and fail if the instance isn't found
--
-- TODO: Use /recordm/instances/{id} instead of definitions/search?
-- Difficulty of ^ is parsing
-- Otherwise:
-- rbody <- performReq (getInstance (fromInteger ref) Nothing) `catchError` throwError ("Couldn't find instance with id " <> show ref)
rmGetInstance :: forall a. Record a => Ref a -> Cob IO a
rmGetInstance ref = snd <$> (rmDefinitionSearch ((byId ref) {_size = 1})) ??? throwError ("Couldn't find instance with id " <> show ref)
{-# INLINE rmGetInstance #-}


-- | Search a @RecordM@ 'Definition' given a 'RecordMQuery', exactly the same as
-- 'rmDefinitionSearch', but ignore the records references ('Ref').
-- Instead, this function returns only a list of the records ('Record') found.
--
-- See also 'rmDefinitionSearch'
-- rmDefinitionSearch_ :: forall a. Record a => Query a -> Cob IO [a]
-- rmDefinitionSearch_ q = map snd <$> rmDefinitionSearch q
-- {-# INLINE rmDefinitionSearch_ #-}

-- | A 'Count' is parametrized with a phantom type @a@ that represents the type
-- of records to count in a definition. Because 'Count' instances 'Num', 'Eq'
-- and 'Ord', it can be used normally as a number and compared against other
-- numbers
newtype Count a = Count { getCount :: Integer -- ^ 'getCount' unwraps the records count value from a 'Count'
                        }

instance Num (Count a) where
    (Count x) + (Count y) = Count (x + y)
    (Count x) - (Count y) = Count (x - y)
    (Count x) * (Count y) = Count (x * y)
    abs (Count x) = Count (abs x)
    signum (Count x) = Count (signum x)
    fromInteger = Count
instance Eq (Count a) where
    (Count x) == (Count y) = x == y
instance Ord (Count a) where
    (Count x) <= (Count y) = x <= y

-- ROMES:TODO: Don't ignore sorting!

-- | Count the number of records matching a query in a definition
rmDefinitionCount :: forall a. Record a => Query a -> Cob IO (Count a)
rmDefinitionCount rmQuery = do
    rbody <- performReq $ searchByName (encode (definition @a)) (Just (_q rmQuery)) (Just 0) (Just 0) Nothing
    count <- parseMaybe (withObject "wO" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody ?? throwError "Couldn't find hits.total.value when doing a definition count"
    return (Count count)

-- ROMES:TODO: Search
-- rmDefinitionSum :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> Cob m (Count a)
-- rmDefinitionSum rmQuery = do
-- {-# INLINABLE rmDefinitionSum #-}

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


-- | Delete an instance by id, ignoring if it has any references
--
-- See /recordm/instances/{id}
rmDeleteInstance :: forall a. Ref a -> Cob IO ()
rmDeleteInstance (Ref ref) = do
  performReq $ deleteInstance (fromInteger ref) (Just True)


-- | @Internal@ Gets hits.hits from response body, parsed as an array of JSON values
getResponseHitsHits :: MonadError e m => Value -> (String -> e) -> m [Value]
getResponseHitsHits responseBody mkError = maybe
        (throwError . mkError $ "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        (either (throwError . mkError) return . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        (parseMaybe (withObject "wO" ((.: "hits") >=> (.: "hits"))) responseBody) -- Find hits.hits in response body
{-# INLINE getResponseHitsHits #-}

