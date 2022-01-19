{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module Cob.RecordM where

import Debug.Trace (trace)
import Control.Lens ((^?))
import qualified Data.Vector as V (fromList)

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)

import Data.Either (either)
import Data.Maybe (catMaybes, mapMaybe)

import Data.Aeson.Types
import Data.Aeson.Lens (key)

import Data.Text as T  (Text, pack)
import Data.ByteString (ByteString)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Char8 as BSC (pack, unpack)

import Network.HTTP.Conduit (Request(..), Response)
import Network.HTTP.Simple (httpJSONEither, JSONException, getResponseStatus, setRequestManager, setRequestBodyJSON, getResponseBody)
import Network.HTTP.Types (urlEncode, renderQuery, simpleQueryToQuery, statusIsSuccessful)

import Cob


--- Definitions and Records
-- | 'Definition' is a type synonym modelling a RecordM Definition
type Definition = ByteString

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
newtype Ref a = Ref Int deriving (Show)
instance ToJSON (Ref a) where
    toJSON (Ref i) = toJSON i
instance FromJSON (Ref a) where
    parseJSON = withObject "record ref" $ \v -> do
        id <- v .: "id"
        return (Ref id)


-- | The standard @RecordM@ query
data StandardRecordMQuery = StandardRecordMQuery { _q         :: Text
                                                 , _from      :: Int
                                                 , _size      :: Int
                                                 , _sort      :: Maybe ByteString
                                                 , _ascending :: Maybe Bool }

-- | Any datatype implementing this typeclass can be used to search @RecordM@.
class RecordMQuery a where
    -- | Convert the implementing type to a 'StandardRecordMQuery'
    toRMQuery :: a -> StandardRecordMQuery

-- | Identity
instance RecordMQuery StandardRecordMQuery where
    toRMQuery = id

-- | Use the default query with argument 'Text' as the query text
instance RecordMQuery Text where
    toRMQuery t = defaultRMQuery { _q = t }

-- | Use the first tuple element to get a 'StandardRecordMQuery', and then use the 'Int' value to set the size
instance RecordMQuery a => RecordMQuery ((,) a Int) where
    toRMQuery (t, i) = (toRMQuery t) { _size = i }

-- | Query for the exact 'Record' using a 'Ref'
instance RecordMQuery (Ref a) where
    toRMQuery (Ref x) = defaultRMQuery { _q = "id:" <> T.pack (show x) }


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
defaultRMQuery :: StandardRecordMQuery
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
rmDefinitionSearch :: forall m a q. (MonadIO m, Record a, RecordMQuery q) => q -> CobT m [(Ref a, a)]
rmDefinitionSearch rmQuery = trace ("search definition " <> BSC.unpack (definition @a)) $ do
    session <- lift ask
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> urlEncode False (definition @a)
                      , queryString = renderRMQuery rmQuery}
    response       <- httpJSONEither request
    rbody :: Value <- unwrapValid response                                        -- Make sure status code is successful 
    hits           <- getResponseHitsHits rbody                                   -- Get hits.hits from response body
    let hitsSources = mapMaybe (^? key "_source") hits                            -- Get _source from each hit
    let ids = mapMaybe (parseMaybe parseJSON) hitsSources                         -- Get id from each _source
    records <- (except . parseEither parseJSON . Array . V.fromList) hitsSources  -- Parse record from each _source
    return (zip ids records)                                                      -- Return list of (id, record)

-- | Search a @RecordM@ 'Definition' given a 'RecordMQuery', exactly the same as
-- 'rmDefinitionSearch', but ignore the records references ('Ref').
-- Instead, this function returns only a list of the records ('Record') found.
--
-- See also 'rmDefinitionSearch'
rmDefinitionSearch_ :: forall m a q. (MonadIO m, Record a, RecordMQuery q) => q -> CobT m [a]
rmDefinitionSearch_ q = map snd <$> rmDefinitionSearch q

-- | Add to @RecordM@ a new instance given a 'Record', and return the 'Ref' of the newly created instance.
--
-- ==== __Example__
--
-- @
-- addDog :: Text -> Text -> Cob (Ref DogsRecord)
-- addDog name ownerName = do
--      rmAddInstance (DogsRecord name ownerName 0)
-- @
rmAddInstance :: forall m a. (MonadIO m, Record a) => a -> CobT m (Ref a)
rmAddInstance record = trace ("add instance to definition " <> BSC.unpack (definition @a)) $ do
    session <- lift ask
    let request = setRequestBodyJSON
                  (object
                      [ "type"   .= decodeUtf8 (definition @a)
                      , "values" .= record ])
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path   = "/recordm/recordm/instances/integration" }
    response       <- httpJSONEither request
    rbody :: Value <- unwrapValid response
    id             <- except . parseEither parseJSON =<< ((rbody ^? key "id") /// throwE "Couldn't get created record id on add instance!")
    return (Ref id)

-- | Update in @RecordM@ an instance given its 'Ref' and the updated 'Record'
rmUpdateInstance :: forall m a. (MonadIO m, Record a) => Ref a -> a -> CobT m ()
rmUpdateInstance (Ref id) record = do
    session <- lift ask
    let request = setRequestBodyJSON
                  (object
                      [ "type"      .= decodeUtf8 (definition @a)
                      , "condition" .= ("id:" <> show id)
                      , "values"    .= record ])
                  (cobDefaultRequest session)
                      { method = "PUT"
                      , path   = "/recordm/recordm/instances/integration" }
    response <- httpJSONEither request
    unwrapValid response :: CobT m Value
    return ()

-- | Get or add an instance given a query and a new 'Record'
rmGetOrAddInstance :: (MonadIO m, Record a, RecordMQuery q) => q -> a -> CobT m (Ref a, a)
rmGetOrAddInstance q = rmGetOrAddInstanceM q . return

-- | Get or add an instance given a query and a new 'Record' inside a 'CobT' monadic context
--
-- The computations to get the new 'Record' will only be executed when no instance matching the query could be found.
-- This means ...
rmGetOrAddInstanceM :: (MonadIO m, Record a, RecordMQuery q) => q -> CobT m a -> CobT m (Ref a, a)
rmGetOrAddInstanceM rmQuery newRecordMIO = do
    session <- lift ask
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




(///) :: Monad m => Maybe a -> m a -> m a
mb /// err = maybe err return mb


--- Util

-- | @Internal@ Render a 'RecordMQuery'.
-- @a@ will be converted with 'toRMQuery' 
renderRMQuery :: RecordMQuery a => a -> ByteString
renderRMQuery q' =
    let q = toRMQuery q' in
        renderQuery True $ simpleQueryToQuery $ catMaybes
            [ Just ("q"   , encodeUtf8 (_q q))
            , Just ("from", BSC.pack $ show $ _from q)
            , Just ("size", BSC.pack $ show $ _size q)
            ,   (,) "sort"  <$> _sort q
            ,   (,) "ascending" . BSC.pack . show <$> _ascending q ]


-- | @Internal@ Gets hits.hits from response body, parsed as an array of JSON values
getResponseHitsHits :: Monad m => Value -> CobT m [Value]
getResponseHitsHits responseBody = maybe
        (throwE "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        (except . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        (responseBody ^? key "hits" . key "hits") -- Find hits.hits in response body


-- | @Internal@ Validate if the status of the response is successful, or throw an exception
unwrapValid :: Monad m => Show a => Response (Either JSONException a) -> CobT m a
unwrapValid r = do
    unless (statusIsSuccessful (getResponseStatus r)) $
        throwE $ "Request failed with status: "
            <> show (getResponseStatus r)
            <> "\nResponse:\n" <> show r
    either (throwE . show) return (getResponseBody r)
