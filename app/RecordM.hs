{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module RecordM where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Functor
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text as T (Text)
import Data.Text.Encoding
import Data.ByteString as BS

import Network.HTTP.Conduit as Net
import Network.HTTP.Simple
import Network.HTTP.Types

import qualified Database.Bloodhound


type Token = ByteString
type Host = ByteString
data Session = Session { tlsmanager :: Manager
                       , serverhost :: Host
                       , cobtoken   :: Cookie }

type DefinitionName = Text
newtype Definition a = Definition DefinitionName

class ToJSON a => Record a where


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

integrationPOST :: Record a => Definition a -> Session -> a -> IO ()
integrationPOST (Definition definitionName) session record = do
    let request = setRequestBodyJSON

                  (object
                      [ "type"   .= definitionName
                      , "values" .= record ])

                  (cobDefaultRequest session)
                      { method = "POST"
                      , path   = "/recordm/recordm/instances/integration" }

    response <- httpJSON request
    print $ "The status code was: " <> show (getResponseStatusCode response)
    print $ toJSON (getResponseBody response :: Value)







-- TODO: How to name this, see bloodhound aggregations

-- definitionSearch :: Session -> DefinitionName -> IO Int
-- definitionSearch session defname = do
--     print $ toJSON $ mkAggregateSearch Nothing $ mkAggregations "users" $ TermsAgg $ mkTermsAggregation "user"

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
    return $ value <$> (result2Maybe =<< (fromJSON <$> x :: Maybe (Result Val)))

    where
    result2Maybe :: Result a -> Maybe a
    result2Maybe (Error _) = Nothing
    result2Maybe (Success x) = Just x
