{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cob.Session
  ( CobToken, Host, CobSession(..), Verbosity(..)
  , withSession, withEmptySession, updateSessionToken
  , tlsManagerFrom
  ) where

import GHC.Conc (newTVarIO)
import System.Log.FastLogger

import Data.String (fromString)

import Data.Time.Clock    (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (Cookie(..), CookieJar, createCookieJar, newManager, Manager)
import Servant.Client as C (ClientEnv(ClientEnv, baseUrl, cookieJar), BaseUrl(..), Scheme(..), defaultMakeClientRequest)
import Control.Monad


-- | A priviledge granting 'CobToken'
type CobToken  = String

-- | A cob server host such as \"test.cultofbits.com\".
type Host      = String

-- | A 'Cob' session, required as an argument to run a 'Cob' computation. It
-- should be created using 'makeSession' unless finer control over the TLS
-- session manager or the cobtoken cookie is needed.
data CobSession
  = CobSession
    { clientEnv :: {-# UNPACK #-} !ClientEnv
    , logger    :: FastLogger
    , verbosity :: Verbosity
    }

-- | The verbosity of logs for the CoB computation using this t'CobSession'.
--
-- To set the verbosity use record update syntax on an existing session,
-- created with a function such as 'withSession'.
--
-- By default, verbosity 'INFO' is used.
data Verbosity
  = ERROR
  | WARN
  | INFO
  | DEBUG

-- | Make a 'CobSession' with the default TLS Manager given the 'Host' and a
-- 'CobToken'.
withSession :: Host -> CobToken -> (CobSession -> IO a) -> IO a
withSession cobhost tok run = withEmptySession cobhost (run <=< (`updateSessionToken` tok))

-- | Create an empty 'CobSession', i.e. without a token
withEmptySession :: Host -> (CobSession -> IO a) -> IO a
withEmptySession cobhost run = withFastLogger (LogStderr defaultBufSize) $ \logger -> do
  manager <- newManager tlsManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Https cobhost 443 "") Nothing defaultMakeClientRequest
  run CobSession{clientEnv, logger, verbosity=INFO}

-- | Update the 'CobToken' of a 'CobSession'
updateSessionToken :: CobSession -> CobToken -> IO CobSession
updateSessionToken sess@CobSession{clientEnv} tok = do
  tvar    <- newTVarIO (makeCookieJar (baseUrlHost $ baseUrl clientEnv) tok)
  return sess{clientEnv=clientEnv{ C.cookieJar = Just tvar }}

makeCookieJar :: Host -> CobToken -> CookieJar
makeCookieJar cobhost tok = createCookieJar
      [Cookie { cookie_name             = "cobtoken"
              , cookie_value            = fromString tok
              , cookie_secure_only      = True
              , cookie_path             = "/"
              , cookie_domain           = fromString cobhost
              , cookie_expiry_time      = future
              , cookie_creation_time    = past
              , cookie_last_access_time = past
              , cookie_persistent       = True
              , cookie_host_only        = False
              , cookie_http_only        = False }]
  where
    past   = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

-- | Get the TLS 'Manager' from a 'CobSession'
tlsManagerFrom :: CobSession -> Manager
tlsManagerFrom CobSession{clientEnv=(ClientEnv manager _ _ _)} = manager
