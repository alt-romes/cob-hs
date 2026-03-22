{-# LANGUAGE ConstraintKinds #-}
module Cob.Utils where

import Data.Aeson.Types

import Control.Monad.IO.Class
import Control.Monad.Reader

import Control.Exception

import qualified Servant.Client

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
      Left e -> throwIO e
      Right b -> return b

-- | Parse a JSON value with a 'Data.Aeson.Types.Parser'. If the parser fails,
-- an exception of type 'JSONParserError' is thrown.
parseOrThrowIO :: MonadIO m => (a -> Parser b) -> a -> m b
parseOrThrowIO parser x =
  case parseEither parser x of
    Left e -> liftIO (throwIO (JSONParserError e))
    Right v -> pure v

newtype JSONParserError = JSONParserError String deriving Show
instance Exception JSONParserError

