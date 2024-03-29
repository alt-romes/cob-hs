{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM
  ( module Cob.UserM
    -- ** Re-exports
  , MonadCob
  ) where

import Control.Exception

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BS
import Data.Aeson (withObject, (.:))

import Control.Monad.IO.Class
import Control.Monad.Reader

import Servant.Client

import Cob.Exception

import qualified Cob.UserM.Servant as Servant
import Cob.UserM.Entities
import Cob.Utils
import Cob.Session
import Cob.Ref
import Cob.Log

-- | UserM writes the added users thorought the Cob computation.
-- This allows it to undo (delete) all added users in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runUserMTests', all added instances will be deleted
-- type instance CobWriter 'UserM = Ref User

-- | Create an UserM user
--
-- Throws the 'NonUniqueUser' 'CobException' if the created user is non unique
-- (a user with the same username already exists).
createUser :: MonadCob m => User -> m (Ref User)
createUser user = do
  logInfo $ "Creating new user with username " <> toLogStr (uusername user)
  CobSession{clientEnv} <- ask
  liftIO $ Servant.Client.runClientM (Servant.createUser user) clientEnv >>= \case
    Left e@(FailureResponse _ (responseBody -> resp)) ->
      if "\"errorType\":\"NON_UNIQUE\"" `BS.isInfixOf` BS.toStrict resp
         then throwIO (NonUniqueUser (uusername user))
         else throwIO e
    Left e -> throwIO e
    Right b -> pure b

-- | Delete an UserM user
deleteUser :: MonadCob m => Ref User -> m ()
deleteUser r@(Ref _ ref) = do
  logInfo $ "Deleting user " <> toLogStr (show r)
  _ <- performReq $ Servant.deleteUser ref
  pure ()

-- | Add users to a group given their ids
addToGroup :: MonadCob m => [Ref User] -> Ref Group -> m ()
addToGroup users r@(Ref _ group) = do
  logInfo $ "Adding users " <> toLogStr (show users) <> " to group " <> toLogStr (show r)
  _ <- performReq $ Servant.addUsersToGroup group users
  pure ()

-- | Log-in to UserM using a username and password.
-- Returns a temporary auth token for the logged in user
umLogin :: MonadCob m => String -> String -> m String
umLogin username pass = do
  logInfo $ "Logging in as " <> toLogStr username
  body <- performReq $ Servant.login (Servant.LoginData username pass)
  parseOrThrowIO (withObject "um_login" (.: "securityToken")) body

-- | Log-in to UserM using a username and password.
-- Returns a temporary 'CobSession' for the logged in user
--
-- Useful for interactive sessions e.g. with GHCi
withUMSession :: Host
          -> String -- ^ Username
          -> String -- ^ Password
          -> (CobSession -> IO a)
          -> IO a
withUMSession hostname username pass run =
    withEmptySession hostname $ \session -> do
      tok     <- runReaderT (umLogin username pass) session
      run =<< updateSessionToken session tok

