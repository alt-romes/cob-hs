{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Aeson (withObject, (.:))

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
createUser :: (MonadReader CobSession m, MonadIO m) => User -> m (Ref User)
createUser user = performReq $ Servant.createUser user

-- | Delete an UserM user
deleteUser :: (MonadReader CobSession m, MonadIO m) => Ref User -> m ()
deleteUser (Ref ref) = do
  _ <- performReq $ Servant.deleteUser ref
  pure ()

-- | Add users to a group given their ids
addToGroup :: (MonadReader CobSession m, MonadIO m) => [Ref User] -> Ref Group -> m ()
addToGroup users (Ref group) = do
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

