{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module RecordM where

import Debug.Trace (trace)
import Control.Lens ((^?))
import qualified Data.Vector as V (fromList)

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)

import Data.Maybe (catMaybes, mapMaybe)

import Data.Aeson.Types
import Data.Aeson.Lens (key)

import Data.Text       (Text)
import Data.ByteString (ByteString)

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.HTTP.Conduit as Net
import Network.HTTP.Simple
import Network.HTTP.Types


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

--- Sessions
type CobToken  = ByteString
type Host      = ByteString
-- | A RecordM session, required to use the API as an argument to 'runCob'.
-- It should be created using 'makeSession' unless finer control over the TLS session manager or cobtoken cookie is needed
data RMSession = RMSession { tlsmanager :: Manager
                           , serverhost :: Host
                           , cobtoken   :: Cookie }

-- | Make a `RMSession` with the default @TLS Manager@ given the `Host` and `CobToken`
makeSession :: Host -> CobToken -> IO RMSession
makeSession host tok = do
    manager <- newManager tlsManagerSettings
    return $ RMSession manager host $ Cookie { cookie_name             = "cobtoken"
                                             , cookie_value            = tok
                                             , cookie_secure_only      = True
                                             , cookie_path             = "/"
                                             , cookie_domain           = host
                                             , cookie_expiry_time      = future
                                             , cookie_creation_time    = past
                                             , cookie_last_access_time = past
                                             , cookie_persistent       = True
                                             , cookie_host_only        = False
                                             , cookie_http_only        = False }
    where
    past   = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)


-- | The default http request used internally (targeting RecordM) in this module, given an RMSession.
-- (Session managed TLS to session host:443 with session's cobtoken)
cobDefaultRequest :: RMSession -> Request
cobDefaultRequest session =
    setRequestManager (tlsmanager session) $
    Net.defaultRequest { secure    = True
                       , port      = 443
                       , host      = serverhost session
                       , cookieJar = Just $ createCookieJar [cobtoken session] }



-- | A RecordM query
data RecordMQuery = RecordMQuery { _q         :: Text
                                 , _from      :: Int
                                 , _size      :: Int
                                 , _sort      :: Maybe ByteString
                                 , _ascending :: Maybe Bool }

-- | The default RecordM query
--      * q = "*"
--      * from = 0
--      * size = 5
--      * sort = 'Nothing'
--      * ascending = 'Nothing'
--
-- ==== __Example__
--
-- @
-- rmDefinitionSearch (defaultRMQuery { _q = "id:123"
--                                    , _from = 1
--                                    , _size = 21
--                                    , _sort = Just \"id\"
--                                    , _ascending = Just True })
-- @
defaultRMQuery :: RecordMQuery
defaultRMQuery = RecordMQuery { _q         = "*"
                              , _from      = 0
                              , _size      = 5
                              , _sort      = Nothing
                              , _ascending = Nothing }

-- | Render a 'RecordMQuery'
renderRMQuery :: RecordMQuery -> ByteString
renderRMQuery q = renderQuery True $ simpleQueryToQuery $ catMaybes
    [ Just ("q"   , encodeUtf8 (_q q))
    , Just ("from", BSC.pack $ show $ _from q)
    , Just ("size", BSC.pack $ show $ _size q)
    ,   (,) "sort"  <$> _sort q
    ,   (,) "ascending" . BSC.pack . show <$> _ascending q ]


--- Cob Monad Stack (TODO: rename to RecordM?)
type RMError = String
type CobT m a = ExceptT RMError (ReaderT RMSession m) a
type Cob a = CobT IO a

-- | Lift a computation from the argument monad to constructed 'CobT' monad
liftCobT :: Monad m => m a -> CobT m a
liftCobT = lift . lift

-- | The inverse of 'CobT'
runCobT :: Monad m => RMSession -> CobT m a -> m (Either RMError a)
runCobT session cob = runReaderT (runExceptT cob) session

-- | Search a 'Definition' given a 'RecordMQuery'
rmDefinitionSearch :: forall m a. (MonadIO m, Record a) => RecordMQuery -> CobT m [(Ref a, a)]
rmDefinitionSearch rmQuery = trace ("search definition " <> BSC.unpack (definition @a)) $ do
    session <- lift ask
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> urlEncode False (definition @a)
                      , queryString = renderRMQuery rmQuery}
    response :: (Response Value) <- httpJSON request
    validateStatusCode response                                                  -- Make sure status code is successful
    hits <- getResponseHitsHits response                                         -- Get hits.hits from response body
    let hitsSources = mapMaybe (^? key "_source") hits                           -- Get _source from each hit
    let ids = mapMaybe (parseMaybe parseJSON) hitsSources                        -- Get id from each _source
    records <- (except . parseEither parseJSON . Array . V.fromList) hitsSources -- Parse record from each _source
    return (zip ids records)                                                     -- Return list of (id, record)


-- | Add an instance given a new 'Record'
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
    -- TODO: Move to httpJSONEither
    -- response :: Response Value <- httpJSONEither request
    response :: Response Value <- httpJSON request
    validateStatusCode response
    id <- except . parseEither parseJSON =<< ((getResponseBody response ^? key "id") /// throwE "Couldn't get created record id on add instance!")
    return (Ref id)

-- | Update an instance given its 'Ref' and the updated 'Record'
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
    response :: Response Value <- httpJSON request
    validateStatusCode response
    return ()

-- | Get or add an instance given a query and a new 'Record'
rmGetOrAddInstance :: (MonadIO m, Record a) => Text -> a -> CobT m (Ref a, a)
rmGetOrAddInstance q = rmGetOrAddInstanceM q . return

-- | Get or add an instance given a query and a new 'Record' inside a 'CobT' monadic context
rmGetOrAddInstanceM :: (MonadIO m, Record a) => Text -> CobT m a -> CobT m (Ref a, a)
rmGetOrAddInstanceM rmQuery newRecordMIO = do
    session <- lift ask
    records <- rmDefinitionSearch (defaultRMQuery { _q = rmQuery })
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



--- Util

-- Gets hits.hits from response body, parsed as an array of JSON values
getResponseHitsHits :: Monad m => Response Value -> CobT m [Value]
getResponseHitsHits response = maybe
        (throwE "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        (except . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        (getResponseBody response ^? key "hits" . key "hits") -- Find hits.hits in response body

validateStatusCode :: Monad m => Show a => Response a -> CobT m ()
validateStatusCode r = unless (statusIsSuccessful (getResponseStatus r)) $
                         throwE $ "Request failed with status: "
                         <> show (getResponseStatus r)
                         <> "\nResponse:\n" <> show r

(///) :: Monad m => Maybe a -> m a -> m a
mb /// err = maybe err return mb
