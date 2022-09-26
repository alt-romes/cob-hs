{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS
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
createUser :: (MonadReader CobSession m, MonadIO m) => User -> m (Ref User)
createUser user = do
  CobSession session <- ask
  liftIO $ Servant.Client.runClientM (Servant.createUser user) session >>= \case
    Left e@(FailureResponse _ (responseBody -> resp)) ->
      if "\"errorType\":\"NON_UNIQUE\"" `BS.isInfixOf` (BS.toStrict resp)
         then throwIO (NonUniqueUser (uusername user))
         else throwIO e
    Left e -> throwIO e
    Right b -> pure b

-- | Delete an UserM user
deleteUser :: (MonadReader CobSession m, MonadIO m) => Ref User -> m ()
deleteUser (Ref _ ref) = do
  _ <- performReq $ Servant.deleteUser ref
  pure ()

-- | Add users to a group given their ids
addToGroup :: (MonadReader CobSession m, MonadIO m) => [Ref User] -> Ref Group -> m ()
addToGroup users (Ref _ group) = do
  _ <- performReq $ Servant.addUsersToGroup group users
  pure ()

-- | Log-in to UserM using a username and password.
-- Returns a temporary auth token for the logged in user
umLogin :: (MonadReader CobSession m, MonadIO m) => String -> String -> m String
umLogin username pass = do
  body <- performReq $ Servant.login (Servant.LoginData username pass)
  parseOrThrowIO (withObject "um_login" (.: "securityToken")) body

-- | Log-in to UserM using a username and password.
-- Returns a temporary 'CobSession' for the logged in user
--
-- Useful for interactive sessions e.g. with GHCi
umSession :: Host
          -> String -- ^ Username
          -> String -- ^ Password
          -> IO CobSession
umSession hostname username pass = do
    session <- emptySession hostname
    tok     <- runReaderT (umLogin username pass) session
    updateSessionToken session tok

