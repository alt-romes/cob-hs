{-# LANGUAGE TupleSections, TemplateHaskell, ScopedTypeVariables, DeriveGeneric, OverloadedStrings #-}
module RecordM where

import Debug.Trace
import GHC.Generics
import Control.Lens hiding ((.=))
import qualified Data.Vector as V
import Data.Bifunctor
import Data.Functor
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Maybe

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text as T (Text)
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.HTTP.Conduit as Net
import Network.HTTP.Simple
import Network.HTTP.Types

-- import qualified Database.Bloodhound


(///) :: Monad m => Maybe a -> m a -> m a
mb /// err = maybe err return mb

--- Definitions and Records
type DefinitionName = ByteString
newtype Definition a = Definition DefinitionName

class (ToJSON a, FromJSON a) => Record a where

--- Record Reference (Id)
newtype Ref a = Ref Int deriving (Show)
instance ToJSON (Ref a) where
    toJSON (Ref i) = toJSON i
instance FromJSON (Ref a) where
    parseJSON = withObject "record ref" $ \v -> do
        id <- v .: "id"
        return (Ref id)

type RMError = String

--- Sessions
type Token = ByteString
type Host = ByteString
data Session = Session { tlsmanager :: Manager
                       , serverhost :: Host
                       , cobtoken   :: Cookie }

makeSession :: Host -> Token -> IO Session
makeSession host tok = do
    manager <- newManager tlsManagerSettings
    return $ Session manager host $ Cookie { cookie_name             = "cobtoken"
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
    past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)


cobDefaultRequest :: Session -> Request
cobDefaultRequest session =
    setRequestManager (tlsmanager session) $
    Net.defaultRequest { secure    = True
                       , port      = 443
                       , host      = serverhost session
                       , cookieJar = Just $ createCookieJar [cobtoken session] }



--- Definition Search
data RecordMQuery = RecordMQuery { _q         :: Text
                                 , _from      :: Int
                                 , _size      :: Int
                                 , _sort      :: Maybe Text
                                 , _ascending :: Maybe Bool }
makeLenses ''RecordMQuery

defaultRMQuery :: RecordMQuery
defaultRMQuery = RecordMQuery { _q         = "*"
                              , _from      = 0
                              , _size      = 5
                              , _sort      = Nothing
                              , _ascending = Nothing }

renderRMQuery :: RecordMQuery -> ByteString
renderRMQuery q = renderQuery True $ simpleQueryToQuery $ catMaybes
    [ Just ("q", encodeUtf8 $ _q q)
    , Just ("from", BSC.pack $ show $ _from q)
    , Just ("size", BSC.pack $ show $ _size q)
    , (,) "sort" . encodeUtf8 <$> _sort q
    , (,) "ascending" . BSC.pack . show <$> _ascending q ]


--- Get or add instance 
rmGetOrAddInstanceM :: (MonadIO m, Record a) => Session -> Definition a -> Text -> ExceptT RMError m a -> ExceptT RMError m (Ref a, a)
rmGetOrAddInstanceM session definition rmQuery newRecordMIO = do
    records <- rmDefinitionSearch session definition (defaultRMQuery & q .~ rmQuery)
    case records of
      [] -> do
          newRecord <- newRecordMIO -- Execute IO actions to retrieve value
          (, newRecord) <$> rmAddInstance session definition newRecord
      record:_ -> return record

rmGetOrAddInstance :: (MonadIO m, Record a) => Session -> Definition a -> Text -> a -> ExceptT RMError m (Ref a, a)
rmGetOrAddInstance s d q newRecord = rmGetOrAddInstanceM s d q (return newRecord)


--- Search definition
rmDefinitionSearch :: (MonadIO m, Record a) => Session -> Definition a -> RecordMQuery -> ExceptT RMError m [(Ref a, a)]
rmDefinitionSearch session (Definition defName) rmQuery = trace ("search definition " <> BSC.unpack defName) $ do
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> urlEncode False defName
                      , queryString = renderRMQuery rmQuery}
    response :: (Response Value) <- httpJSON request
    validateStatusCode response                                                  -- Make sure status code is successful
    hits <- getResponseHitsHits response                                         -- Get hits.hits from response body
    let hitsSources = mapMaybe (^? key "_source") hits                           -- Get _source from each hit
    let ids = mapMaybe (parseMaybe parseJSON) hitsSources                        -- Get id from each _source
    records <- (except . parseEither parseJSON . Array . V.fromList) hitsSources -- Parse record from each _source
    return (zip ids records)                                                     -- Return list of (id, record)
            
--- Add instance
rmAddInstance :: (MonadIO m, Record a) => Session -> Definition a -> a -> ExceptT RMError m (Ref a)
rmAddInstance session (Definition defName) record = trace ("add instance to definition " <> BSC.unpack defName) $ do
    let request = setRequestBodyJSON
                  (object
                      [ "type"   .= decodeUtf8 defName
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

--- Update instance (should individual instance update be done through integration?
rmUpdateInstance :: (MonadIO m, Record a) => Session -> Definition a -> Ref a -> a -> ExceptT RMError m ()
rmUpdateInstance session (Definition defName) (Ref id) record = do
    let request = setRequestBodyJSON
                  (object
                      [ "type"      .= decodeUtf8 defName
                      , "condition" .= ("id:" <> show id)
                      , "values"    .= record ])
                  (cobDefaultRequest session)
                      { method = "PUT"
                      , path   = "/recordm/recordm/instances/integration" }
    response :: Response Value <- httpJSON request
    validateStatusCode response
    return ()


newtype Val = Val {
    value :: Int
} deriving (Generic)
instance FromJSON Val

definitionSearch :: Session -> DefinitionName -> Value -> IO (Maybe Int)
definitionSearch session defname query = do
    print query
    let request = setRequestBodyJSON query
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path   = "/recordm/recordm/definitions/search/advanced/11?size=0" }

    response <- httpJSON request :: IO (Response Value)
    print response
    let x = getResponseBody response ^? key "aggregations" . key "sum#soma"
    print x
    return $ value <$> (parseMaybe parseJSON =<< x )



--- Util

-- Gets hits.hits from response body, parsed as an array of JSON values
getResponseHitsHits :: (Monad m, AsValue a) => Response a -> ExceptT RMError m [Value]
getResponseHitsHits response = maybe
        (throwE "Couldn't find hits.hits in response body!")  -- Error message for when hits.hits doesn't exist
        (except . parseEither parseJSON)                      -- Parse [Value] when JSON hits.hits exists
        (getResponseBody response ^? key "hits" . key "hits") -- Find hits.hits in response body

validateStatusCode :: Monad m => Show a => Response a -> ExceptT RMError m ()
validateStatusCode r = unless (statusIsSuccessful (getResponseStatus r)) $
                           throwE $ "Request failed with status: "
                           <> show (getResponseStatus r)
                           <> "\nResponse:\n" <> show r

