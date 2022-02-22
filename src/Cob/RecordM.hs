{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob.RecordM where

import Debug.Trace                ( trace      )
import Control.Lens               ( (^?)       )
import qualified Data.Vector as V ( fromList   )

import Control.Monad              ( unless, forM, join, mapM    )
import Control.Monad.Reader       ( ask                         )
import Control.Monad.Except       ( throwError                  )
import Control.Monad.Trans        ( MonadIO, lift               )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT         )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, except )

import Data.Bifunctor     ( second                           )
import Data.Either        ( either                           )
import Data.Maybe         ( catMaybes, mapMaybe, listToMaybe )

import Data.Aeson.Types
import Data.Aeson.Lens    ( key, _Integer )

import Data.Text          ( Text       )
import Data.ByteString    ( ByteString )
import Data.String        ( fromString )
import Data.Text.Encoding ( encodeUtf8 )
import Network.URI.Encode ( encode     )

import Network.HTTP.Conduit ( Request(..), Response                                                                                    )
import Network.HTTP.Simple  ( httpJSONEither, JSONException, getResponseStatus, setRequestManager, setRequestBodyJSON, getResponseBody )
import Network.HTTP.Types   ( renderQuery, simpleQueryToQuery, statusIsSuccessful                                                      )

import Cob


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

-- | A 'Ref' of a 'Record' holds the RecordM /id/ of another record
-- 
-- ==== __Example__
--
-- Taking the above /Dogs/ definition,
-- and modifying the /Owner Name/ field to a reference to a different table called /Owners/,
-- the @DogsRecord@ must be modified with `Ref` to reflect this.
--
-- @
-- data OwnersRecord ...
-- data DogsRecord = DogsRecord Text (Ref OwnersRecord) Int
--
-- instance ToJSON DogsRecord where
--     toJSON (DogsRecord name (Ref ownerId) age) = object
--         [ \"Name\" .= name
--         , \"Owner name\" .= show ownerId
--         , \"Age\" .= age ]
-- instance FromJSON DogsRecord where
--     parseJSON = withObject "user record" $ \v -> do
--         [name] <- v .: "name"
--         [ownerId] <- v .: "owner_name"
--         [age] <- v .: "age"
--         return (DogsRecord name (Ref (read ownerId)) (read age))
-- @
newtype Ref a = Ref { ref_id :: Int }
instance Show (Ref a) where
    show (Ref x) = show x
instance ToJSON (Ref a) where
    toJSON (Ref i) = toJSON i
instance FromJSON (Ref a) where
    parseJSON = withObject "record ref" $ \v -> do
        id <- v .: "id"
        return (Ref id)


-- | The standard @RecordM@ query
data StandardRecordMQuery a = StandardRecordMQuery { _q         :: Text
                                                   , _from      :: Int
                                                   , _size      :: Int
                                                   , _sort      :: Maybe ByteString
                                                   , _ascending :: Maybe Bool
                                                   } deriving (Show)

-- | Any datatype implementing this typeclass can be used to search @RecordM@.
class Record a => RecordMQuery q a where
    -- | Convert the implementing type to a 'StandardRecordMQuery'
    toRMQuery :: q -> StandardRecordMQuery a

-- | Identity
instance Record a => RecordMQuery (StandardRecordMQuery a) a where
    toRMQuery = id

-- | Use the default query with argument 'String as the query text
instance Record a => RecordMQuery String a where
    toRMQuery t = defaultRMQuery { _q = fromString t }

-- | Use the default query with argument 'Text' as the query text
instance Record a => RecordMQuery Text a where
    toRMQuery t = defaultRMQuery { _q = t }

-- | Use the first tuple element to get a 'StandardRecordMQuery', and then use the 'Int' value to set the size
instance RecordMQuery q a => RecordMQuery ((,) q Int) a where
    toRMQuery (t, i) = (toRMQuery @q @a t) { _size = i }

-- | Query for the exact 'Record' using a 'Ref'.
--
-- Note: The definition manipulated is not inferred by the query -- i.e. you could search a Definition X with a @Ref Y@
instance Record a => RecordMQuery (Ref a) a where
    toRMQuery (Ref x) = (defaultRMQuery @a) { _q = "id:" <> fromString (show x) }

-- | The default 'RecordMQuery'
-- @
-- defaultRMQuery = StandardRecordMQuery { _q         = "*"
--                                       , _from      = 0
--                                       , _size      = 5
--                                       , _sort      = Nothing
--                                       , _ascending = Nothing }
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
defaultRMQuery :: StandardRecordMQuery a
defaultRMQuery = StandardRecordMQuery { _q         = "*"
                                      , _from      = 0
                                      , _size      = 5
                                      , _sort      = Nothing
                                      , _ascending = Nothing }



-- | Search a @RecordM@ 'Definition' given a 'RecordMQuery', and return a list of references ('Ref') of a record and the corresponding records ('Record').
--
-- The 'Record' type to search for is inferred from the usage of the return element.
-- When the information available isn't enough to correctly infer the search 'Record' type, an explicit type can be used to help the compiler.
--
-- ==== __Example__
--
-- @
-- dogs :: [(Ref DogsRecord, Dogs)] <- rmDefinitionSearch "bobby"
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
rmDefinitionSearch :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> CobT m [(Ref a, a)]
rmDefinitionSearch rmQuery = trace ("search definition " <> definition @a) $ do
    session <- ask
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> fromString (encode $ definition @a)
                      , queryString = renderRMQuery @q @a rmQuery }
    response       <- httpJSONEither request
    rbody :: Value <- unwrapValid response                                        -- Make sure status code is successful 
    hits           <- getResponseHitsHits rbody                                   -- Get hits.hits from response body
    let hitsSources = mapMaybe (^? key "_source") hits                            -- Get _source from each hit
    let ids = mapMaybe (parseMaybe parseJSON) hitsSources                         -- Get id from each _source
    records <- (CobT . except . parseEither parseJSON . Array . V.fromList) hitsSources  -- Parse record from each _source
    return (zip ids records)                                                      -- Return list of (id, record)

-- | Search a @RecordM@ 'Definition' given a 'RecordMQuery', exactly the same as
-- 'rmDefinitionSearch', but ignore the records references ('Ref').
-- Instead, this function returns only a list of the records ('Record') found.
--
-- See also 'rmDefinitionSearch'
rmDefinitionSearch_ :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> CobT m [a]
rmDefinitionSearch_ q = map snd <$> rmDefinitionSearch q

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

-- | Count the number of records matching a query in a definition
rmDefinitionCount :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> CobT m (Count a)
rmDefinitionCount rmQuery = trace ("definition count " <> definition @a) $ do
    session <- ask
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> fromString (encode $ definition @a)
                      , queryString = renderRMQuery @q @a rmQuery}
    response       <- httpJSONEither request
    rbody :: Value <- unwrapValid response                                        -- Make sure status code is successful 
    count <- rbody ^? key "hits" . key "total" . key "value" . _Integer ?: throwError "Couldn't find hits.total.value when doing a definition count"
    return (Count count)


-- | Add to @RecordM@ a new instance given a 'Record', and return the 'Ref' of the newly created instance.
--
-- ==== __Example__
--
-- @
-- addDog :: Text -> Text -> Cob (Ref DogsRecord)
-- addDog name ownerName = do
--      rmAddInstance (DogsRecord name ownerName 0)
-- @
rmAddInstance :: forall a m. (MonadIO m, Record a) => a -> CobT m (Ref a)
rmAddInstance record = trace ("add instance to definition " <> definition @a) $ do
    session <- CobT $ lift ask
    let request = setRequestBodyJSON
                  (object
                      [ "type"   .= definition @a
                      , "values" .= record ])
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path   = "/recordm/recordm/instances/integration" }
    response       <- httpJSONEither request
    rbody :: Value <- unwrapValid response
    id             <- CobT . except . parseEither parseJSON =<< ((rbody ^? key "id") ?: throwError "Couldn't get created record id on add instance!")
    return (Ref id)

-- TODO: Move update instance and update field to RecordMQuery instead of Ref?

-- | Update an instance with an id and return the updated record.
-- An error will be thrown if no record is successfully updated.
rmUpdateInstance :: forall a m. (MonadIO m, Record a) => Ref a -> (a -> a) -> CobT m a
rmUpdateInstance ref f = rmUpdateInstances_ ref f ?:: throwError ("Updating instance " <> show ref <> " was not successful!")

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
rmUpdateInstances :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> (a -> a) -> CobT m [(Ref a, a)]
rmUpdateInstances q f = rmUpdateInstancesM q (return <$> f)

-- | The same as 'rmUpdateInstance', but the function to update the record returns a 'CobT'
rmUpdateInstancesM :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> (a -> CobT m a) -> CobT m [(Ref a, a)]
rmUpdateInstancesM rmQuery updateRecord = trace ("update instances in definition " <> definition @a) $ do
    records <- trace "Definition might be being searched preemptively for update?" $ rmDefinitionSearch rmQuery
    session <- ask
    trace ("updating n records: " <> show (length records) <> " matchin query q: " <> show (toRMQuery @q @a rmQuery)) $
        forM records $ \(Ref id, rec) -> do
            updatedRecord <- updateRecord rec
            let request = setRequestBodyJSON
                          (object
                              [ "type"      .= definition @a
                              , "condition" .= ("id:" <> show id)
                              , "values"    .= updatedRecord ])
                          (cobDefaultRequest session)
                              { method = "PUT"
                              , path   = "/recordm/recordm/instances/integration" }
            response <- httpJSONEither request
            unwrapValid response :: CobT m Value
            return (Ref id, updatedRecord)

-- | The same as 'rmUpdateInstance' but discard the @'Ref' a@ from @('Ref' a, a)@ from the result
rmUpdateInstances_ :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> (a -> a) -> CobT m [a]
rmUpdateInstances_ q = fmap (map snd) . rmUpdateInstances q

-- | The same as 'rmUpdateInstancesWithMakeQueryM' but the update record function does not return the value within a monad @m@
rmUpdateInstancesWithMakeQuery :: forall a b m q r. (MonadIO m, Record a, Record b, RecordMQuery q a, RecordMQuery r b) => q -> (a -> r) -> (b -> b) -> CobT m [(Ref b, b)]
rmUpdateInstancesWithMakeQuery q f g = rmUpdateInstancesWithMakeQueryM q f (return <$> g)

-- | Run a @'RecordMQuery' q@ and transform all resulting records (@'Record' a@)
-- into new queries (@'RecordMQuery' r@). Finally, update all resulting records
-- (@'Record' b@) with the third argument, the function (@b -> 'CobT' m b@).
rmUpdateInstancesWithMakeQueryM :: forall a b m q r. (MonadIO m, Record a, Record b, RecordMQuery q a, RecordMQuery r b) => q -> (a -> r) -> (b -> CobT m b) -> CobT m [(Ref b, b)]
rmUpdateInstancesWithMakeQueryM rmQuery getRef updateRecord = rmDefinitionSearch_ rmQuery >>= fmap join . mapM (flip rmUpdateInstancesM updateRecord . getRef)

-- | Get or add an instance given a query and a new 'Record'
rmGetOrAddInstance :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> a -> CobT m (Ref a, a)
rmGetOrAddInstance q = rmGetOrAddInstanceM q . return

-- | Get or add an instance given a query and a new 'Record' inside a 'CobT' monadic context
--
-- The computations to get the new 'Record' will only be executed when no instance matching the query could be found.
-- This means ...
rmGetOrAddInstanceM :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> CobT m a -> CobT m (Ref a, a)
rmGetOrAddInstanceM rmQuery newRecordMIO = do
    session <- ask
    records <- rmDefinitionSearch rmQuery
    case records of
      [] -> do
          newRecord <- newRecordMIO -- Execute IO computation to retrieve value
          (, newRecord) <$> rmAddInstance newRecord
      record:_ -> return record


-- newtype Val = Val {
--     value :: Int
-- } deriving (Generic)
-- instance FromJSON Val

-- definitionSearch :: Session -> DefinitionName -> Value -> IO (Maybe Int)
-- definitionSearch session defname query = do
--     print query
--     let request = setRequestBodyJSON query
--                   (cobDefaultRequest session)
--                       { method = "POST"
--                       , path   = "/recordm/recordm/definitions/search/advanced/11?size=0" }

--     response <- httpJSON request :: IO (Response Value)
--     print response
--     let x = getResponseBody response ^? key "aggregations" . key "sum#soma"
--     print x
--     return $ value <$> (parseMaybe parseJSON =<< x )

-- | An 'Existable' @e@ possibly wraps a value and provides an operation '?:'
-- to retrieve the element if it exists or return a default value (passed in
-- the second parameter) if it doesn't.
class Existable e where
    -- | Return the value from the 'Existable' if it exists, otherwise return the
    -- default value passed as the second argument
    (?:) :: Monad m => e a -> m a -> m a
    infix 7 ?:

    -- | Return the value from an 'Existable' in @'Monad' m@ if it exists, otherwise return the
    -- default value passed as the second argument
    (?::) :: Monad m => Existable e => m (e a) -> m a -> m a
    mex ?:: def = mex >>= flip (?:) def
    infix 7 ?::

-- | A 'Maybe' is 'Existable' because it's either 'Just' a value or 'Nothing'.
instance Existable Maybe where
    -- | Return the value from the 'Maybe' if it exists, otherwise return the
    -- default value passed as the second argument
    mb ?: def = maybe def return mb

-- | A list is 'Existable' because it might have no elements or at least one
-- element. When using '?:', if the list is non-empty, the first element will
-- be returned. This is useful when doing a 'rmDefinitionSearch' and are
-- expecting exactly one result, and want to throw an error in the event that
-- none are returned.
--
-- Example:
-- 
-- @
-- user <- rmDefinitionSearch_ @UsersRecord name ?:: throwError "Couldn't find user!"
-- @
instance Existable [] where
    -- | Return the first element of the list if it exists, otherwise return the
    -- default value passed as the second argument
    ls ?: def = (maybe def return . listToMaybe) ls


--- Util

-- | @Internal@ Render a 'RecordMQuery'.
-- @a@ will be converted with 'toRMQuery' 
renderRMQuery :: forall q a. (Record a, RecordMQuery q a) => q -> ByteString
renderRMQuery q' =
    let q = toRMQuery @q @a q' in
        renderQuery True $ simpleQueryToQuery $ catMaybes
            [ Just ("q"   , encodeUtf8 (_q q))
            , Just ("from", fromString $ show $ _from q)
            , Just ("size", fromString $ show $ _size q)
            ,   (,) "sort"  <$> _sort q
            ,   (,) "ascending" . fromString . show <$> _ascending q ]


-- | @Internal@ Gets hits.hits from response body, parsed as an array of JSON values
getResponseHitsHits :: Monad m => Value -> CobT m [Value]
getResponseHitsHits responseBody = CobT $ maybe
        (throwError "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        (except . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        (responseBody ^? key "hits" . key "hits") -- Find hits.hits in response body


-- | @Internal@ Validate if the status of the response is successful, or throw an exception
unwrapValid :: Monad m => Show a => Response (Either JSONException a) -> CobT m a
unwrapValid r = CobT $ do
    unless (statusIsSuccessful (getResponseStatus r)) $
        throwError $ "Request failed with status: "
            <> show (getResponseStatus r)
            <> "\nResponse:\n" <> show r
    either (throwError . show) return (getResponseBody r)
