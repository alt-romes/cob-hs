{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob.Utils where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import Data.Foldable (toList)

import Control.Monad.IO.Class
import Control.Monad.Reader

import Control.Exception

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Servant.Client
import Servant.Client (ClientError(..), BaseUrl(..), Scheme(..))
import Servant.Client.Core.Request (RequestF(..))
import Servant.Client.Core.Response (ResponseF(..))

import Cob.Session

type MonadCob m = (MonadIO m, MonadReader CobSession m)

-- Eventually, we could have
-- class MonadIO m => MonadCob m where
--   askSession :: m CobSession
-- instance MonadIO m => MonadCob (ReaderT CobSession m) where
--   askSession = ask
-- instance MonadCob m => MonadCob (ReaderT env m) where
--   askSession = lift askSession
-- instance MonadCob m => MonadCob (StateT env m) where
--   askSession = lift askSession

-- | Perform a request on a Cob servant api
performReq :: MonadCob m => Servant.Client.ClientM a -> m a
performReq c = do
    CobSession{clientEnv} <- ask
    liftIO $ Servant.Client.runClientM c clientEnv >>= \case
      Left e -> throwIO (PrettyClientError e)
      Right b -> return b

-- | Wrapper around 'Servant.Client.ClientError' whose 'Show' instance
-- pretty-prints 'FailureResponse' (status, headers, decoded JSON body,
-- request method/url) so that uncaught request errors are readable.
newtype PrettyClientError = PrettyClientError ClientError
instance Exception PrettyClientError
instance Show PrettyClientError where
  show (PrettyClientError e) = case e of
    FailureResponse req resp -> prettyFailureResponse req resp
    other -> show other

prettyFailureResponse :: RequestF () (BaseUrl, BS.ByteString) -> ResponseF BL.ByteString -> String
prettyFailureResponse req resp = unlines $
  [ "FailureResponse:"
  , "  " <> show (requestMethod req) <> " " <> showUrl req
  , "  Status: " <> show (responseStatusCode resp)
  ] <> bodyBlock
  where
    bodyBlock =
      let body = responseBody resp
          pretty = case eitherDecode body :: Either String Value of
            Right v -> BL.unpack (encodePretty v)
            Left _  -> BL.unpack body
      in if BL.null body then [] else "  Body:" : map ("    " <>) (lines pretty)
    showUrl r =
      let (BaseUrl sch host port basePath, urlPath) = requestPath r
          scheme = case sch of Http -> "http"; Https -> "https"
          portPart
            | (sch == Http  && port == 80)
              || (sch == Https && port == 443) = ""
            | otherwise = ":" <> show port
          qs = toList (requestQueryString r)
          qsStr
            | null qs = ""
            | otherwise = "?" <> intercalate' "&"
                [ BL.unpack (BL.fromStrict k) <> maybe "" (\v -> "=" <> BL.unpack (BL.fromStrict v)) mv
                | (k, mv) <- qs ]
      in scheme <> "://" <> host <> portPart <> basePath <> BS.unpack urlPath <> qsStr
    intercalate' sep = \case
      [] -> ""
      (x:xs) -> foldl (\acc y -> acc <> sep <> y) x xs

-- | Parse a JSON value with a 'Data.Aeson.Types.Parser'. If the parser fails,
-- an exception of type 'JSONParserError' is thrown.
parseOrThrowIO :: MonadIO m => (a -> Parser b) -> a -> m b
parseOrThrowIO parser x =
  case parseEither parser x of
    Left e -> liftIO (throwIO (JSONParserError e))
    Right v -> pure v

newtype JSONParserError = JSONParserError String deriving Show
instance Exception JSONParserError

