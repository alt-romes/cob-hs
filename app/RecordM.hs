{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module RecordM where

import GHC.Generics
import Control.Lens hiding ((.=))
import qualified Data.Vector as V
import Data.Bifunctor
import Data.Functor
import Control.Monad
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



(//) :: Monad m => m (Maybe a) -> a -> m a
ma // d = fromMaybe d <$> ma

--- Definitions and Records
type DefinitionName = ByteString
newtype Definition a = Definition DefinitionName

class (ToJSON a, FromJSON a) => Record a where

--- Record Reference (Id)
newtype Ref a = Ref Int deriving (Show)
instance ToJSON (Ref a) where
    toJSON (Ref i) = toJSON i
instance FromJSON (Ref a) where
    parseJSON (Object v) = do
        id <- v .: "id"
        return (Ref id)


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
data RecordMQuery = RecordMQuery { rmQ         :: Text
                                 , rmFrom      :: Int
                                 , rmSize      :: Int
                                 , rmSort      :: Maybe Text
                                 , rmAscending :: Maybe Bool }

defaultRMQuery :: RecordMQuery
defaultRMQuery = RecordMQuery { rmQ         = "*"
                              , rmFrom      = 0
                              , rmSize      = 5
                              , rmSort      = Nothing
                              , rmAscending = Nothing }


-- TODO: Handle errors for this and below better: Network errors, parsing errors, etc (Left Error)
rmDefinitionSearch :: (Show a, Record a) => Session -> Definition a -> RecordMQuery -> IO (Maybe [(Ref a, a)])
rmDefinitionSearch session (Definition defName) rmQuery = do
    let request = (cobDefaultRequest session)
                      { path = "/recordm/recordm/definitions/search/name/" <> urlEncode False defName
                      , queryString = renderRMQuery rmQuery}
    response <- httpJSON request :: IO (Response Value)
    print response
    let hits = parseMaybe parseJSON =<< (getResponseBody response ^? key "hits" . key "hits") :: Maybe [Value]
    let hitsSources = mapMaybe (^? key "_source") <$> hits -- Sources are then parsed into records
    let ids = mapMaybe (parseMaybe parseJSON) <$> hitsSources :: Maybe [Ref a]
    print $ ids
    let records = parseMaybe parseJSON . Array . V.fromList =<< hitsSources :: Record a => Maybe [a]
    print $ records
    print $ "The status code was: " <> show (getResponseStatusCode response)
    return $ zip <$> ids <*> records

    where
    renderRMQuery :: RecordMQuery -> ByteString
    renderRMQuery q = renderQuery True $ simpleQueryToQuery $ catMaybes
        [ Just ("q", encodeUtf8 $ rmQ q)
        , Just ("from", BSC.pack $ show $ rmFrom q)
        , Just ("size", BSC.pack $ show $ rmSize q)
        , (,) "sort" . encodeUtf8 <$> rmSort q
        , (,) "ascending" . BSC.pack . show <$> rmAscending q ]

            
--- Add instance
rmAddInstance :: Record a => Session -> Definition a -> a -> IO (Maybe (Ref a))
rmAddInstance session (Definition defName) record = do
    let request = setRequestBodyJSON
                  (object
                      [ "type"   .= decodeUtf8 defName
                      , "values" .= record ])
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path   = "/recordm/recordm/instances/integration" }
    response <- httpJSON request :: IO (Response Value)
    let id = parseMaybe parseJSON =<< (getResponseBody response ^? key "id") 
    return (Ref <$> id)

-- TODO: Propagate errors

--- Update instance (should individual instance update be done through integration?
rmUpdateInstance :: Record a => Session -> Definition a -> Ref a -> a -> IO ()
rmUpdateInstance session (Definition defName) (Ref id) record = do
    let request = setRequestBodyJSON
                  (object
                      [ "type"      .= decodeUtf8 defName
                      , "condition" .= ("id:" <> show id)
                      , "values"    .= record ])
                  (cobDefaultRequest session)
                      { method = "PUT"
                      , path   = "/recordm/recordm/instances/integration" }
    response <- httpJSON request :: IO (Response Value)
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





-- TODO: How to name this, see bloodhound aggregations

-- definitionSearch :: Session -> DefinitionName -> IO Int
-- definitionSearch session defname = do
--     print $ toJSON $ mkAggregateSearch Nothing $ mkAggregations "users" $ TermsAgg $ mkTermsAggregation "user"

