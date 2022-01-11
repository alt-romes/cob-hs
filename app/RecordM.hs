{-# LANGUAGE OverloadedStrings #-}
module RecordM where

import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text as T (Text)
import Data.Text.Encoding
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types
import Data.ByteString as BS

type DefinitionName = Text
type Token = ByteString
type Host = ByteString
data Session = Session { tlsmanager :: Manager
                       , serverhost :: Host
                       , cobtoken   :: Cookie }

class ToJSON a => Record a where
    definitionName :: a -> DefinitionName


past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

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


integrationPOST :: Record a => Session -> a -> IO ()
integrationPOST session record = do
    let request = setRequestManager (tlsmanager session) $
                  setRequestBodyJSON
                  (object [ "type"   .= definitionName record
                          , "values" .= record ])
                  defaultRequest { secure = True
                                 , method = "POST"
                                 , host = serverhost session
                                 , path = "/recordm/recordm/instances/integration"
                                 , port = 443
                                 -- , requestVersion = http20
                                 , cookieJar = Just $ createCookieJar [cobtoken session] }
    response <- httpJSON request
    print $ "The status code was: " <> show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    print $ toJSON (getResponseBody response :: Value)


