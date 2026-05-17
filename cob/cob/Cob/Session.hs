{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cob.Session
  ( CobToken, Host, CobSession(..), Verbosity(..)
  , withSession, withEmptySession, updateSessionToken
  , tlsManagerFrom
  ) where

import GHC.Conc (newTVarIO)
import System.Log.FastLogger

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String (fromString)

import Data.Time.Clock    (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BB
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client
  ( Cookie(..), CookieJar, createCookieJar, newManager, Manager
  , ManagerSettings(..), Request, RequestBody(..)
  , host, port, secure, method, path, queryString, requestHeaders, requestBody
  )
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

-- | Make a 'CobSession' with the default TLS Manager given the 'Host' and a 'CobToken'.
--
-- Note: this session is not like a TLS session in the sense that is maintained actively.
-- Within the scope of this session the 'CobSession' can be used to establish
-- connections to and use RecordM, and the logger's lifetime is delimited by
-- it.
withSession :: Host -> CobToken -> (CobSession -> IO a) -> IO a
withSession cobhost tok run = withEmptySession cobhost (run <=< (`updateSessionToken` tok))

-- | Create an empty 'CobSession', i.e. without a token
withEmptySession :: Host -> (CobSession -> IO a) -> IO a
withEmptySession cobhost run = withFastLogger (LogStderr defaultBufSize) $ \logger -> do
  let settings = tlsManagerSettings
        { managerModifyRequest = \req -> logRequest logger req >> pure req }
  manager <- newManager settings
  let clientEnv = ClientEnv manager (BaseUrl Https cobhost 443 "") Nothing defaultMakeClientRequest id
  run CobSession{clientEnv, logger, verbosity=INFO}

-- | Log every outgoing client request as a reproducible @curl@ command via the
-- session 'FastLogger'. Wired in as a 'managerModifyRequest' hook so it sees
-- the fully-serialised http-client 'Request' (cookies already injected as
-- @Cookie:@ headers) moments before it hits the wire.
logRequest :: FastLogger -> Request -> IO ()
logRequest logger req = do
  body <- renderBody (requestBody req)
  let url = (if secure req then "https://" else "http://")
         <> BS.unpack (host req)
         <> portPart
         <> BS.unpack (path req)
         <> BS.unpack (queryString req)
      portPart =
        let p = port req
            sec = secure req
        in if (sec && p == 443) || (not sec && p == 80) then "" else ':' : show p
      headerArgs = concat
        [ [" -H ", shellQuote (BS.unpack (foldedHeader n v))]
        | (n, v) <- requestHeaders req ]
      bodyArg = case body of
        Nothing -> ""
        Just b  -> " --data-raw " <> shellQuote b
      cmd = concat $
        [ "curl -i -X ", BS.unpack (method req)
        , " ", shellQuote url
        ] <> headerArgs <> [bodyArg]
  logger $ toLogStr ("[cob-client] " <> cmd <> "\n")
  where
    foldedHeader n v = BS.pack (show n) <> ": " <> v

    shellQuote s = '\'' : concatMap esc s <> "'"
      where esc '\'' = "'\\''"
            esc c    = [c]

    renderBody :: RequestBody -> IO (Maybe String)
    renderBody = \case
      RequestBodyLBS lbs   -> pure $ Just (BL.unpack lbs)
      RequestBodyBS  bs    -> pure $ Just (BS.unpack bs)
      RequestBodyBuilder _ b -> pure $ Just (BL.unpack (BB.toLazyByteString b))
      RequestBodyStream _ _        -> pure $ Just "<stream body>"
      RequestBodyStreamChunked _   -> pure $ Just "<chunked stream body>"
      RequestBodyIO act            -> act >>= renderBody

-- | Update the 'CobToken' of a 'CobSession'
updateSessionToken :: CobSession -> CobToken -> IO CobSession
updateSessionToken sess@CobSession{clientEnv} tok = do
  tvar    <- newTVarIO (makeCookieJar (baseUrlHost $ baseUrl clientEnv) tok)
  return sess{clientEnv=clientEnv{ C.cookieJar = Just tvar }}

makeCookieJar :: Host -> CobToken -> CookieJar
makeCookieJar cobhost tok = createCookieJar
      -- Strip surrounding whitespace: a stray '\n' (e.g. from `readFile`) inside
      -- can result in really weird NGINX 400s (e.g. newline makes rest of
      -- request be considered a new (malformed) HTTP request).
      [Cookie { cookie_name             = "cobtoken"
              , cookie_value            = fromString (dropWhile isSpace (dropWhileEnd isSpace tok))
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
tlsManagerFrom CobSession{clientEnv=ClientEnv manager _ _ _ _} = manager
