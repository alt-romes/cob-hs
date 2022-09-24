{-# LANGUAGE OverloadedStrings #-}
module Cob.Session
  ( CobToken, Host, CobSession(..)
  , makeSession, emptySession, updateSessionToken
  , tlsManagerFrom
  ) where

import GHC.Conc (newTVarIO)

import Data.String (fromString)

import Data.Time.Clock    (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (Cookie(..), CookieJar, createCookieJar, newManager, Manager)
import Servant.Client as C (ClientEnv(ClientEnv, baseUrl, cookieJar), BaseUrl(..), Scheme(..), defaultMakeClientRequest)


-- | A priviledge granting 'CobToken'
type CobToken  = String

-- | A cob server host such as \"test.cultofbits.com\".
type Host      = String

-- | A 'Cob' session, required as an argument to run a 'Cob' computation. It
-- should be created using 'makeSession' unless finer control over the TLS
-- session manager or the cobtoken cookie is needed.
newtype CobSession = CobSession ClientEnv

-- | Make a 'CobSession' with the default TLS Manager given the 'Host' and a
-- 'CobToken'.
makeSession :: Host -> CobToken -> IO CobSession
makeSession cobhost tok = do
    manager <- newManager tlsManagerSettings
    tvar    <- newTVarIO (makeCookieJar cobhost tok)
    return $ CobSession $ ClientEnv manager (BaseUrl Https cobhost 443 "") (Just tvar) defaultMakeClientRequest

-- | Create an empty 'CobSession', i.e. without a token
emptySession :: Host -> IO CobSession
emptySession cobhost = do
  manager <- newManager tlsManagerSettings
  return $ CobSession $ ClientEnv manager (BaseUrl Https cobhost 443 "") Nothing defaultMakeClientRequest

-- | Update the 'CobToken' of a 'CobSession'
updateSessionToken :: CobSession -> CobToken -> IO CobSession
updateSessionToken (CobSession clEnv) tok = do
  tvar    <- newTVarIO (makeCookieJar (baseUrlHost $ baseUrl clEnv) tok)
  return (CobSession (clEnv { C.cookieJar = Just tvar }))

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
tlsManagerFrom (CobSession (ClientEnv manager _ _ _)) = manager
