{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM
  ( module Cob.UserM
  , module Cob.UserM.Types ) where

import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)

import Data.DList  (singleton)

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseMaybe)

import Cob.UserM.Types
import qualified Cob.UserM.Servant as Servant
import Cob

-- | UserM writes the added users thorought the Cob computation.
-- This allows it to undo (delete) all added users in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runUserMTests', all added instances will be deleted
type instance CobWriter 'UserM = UMRef UMUser

-- | Create an UserM user
umCreateUser :: UMUser -> Cob IO (UMRef UMUser)
umCreateUser user = do
  ref <- performReq $ Servant.createUser user
  tell (mempty, singleton ref)
  return ref

-- | Add users to a group given their ids
umAddUsersToGroup :: [UMRef UMUser] -> UMRef UMGroup -> Cob IO ()
umAddUsersToGroup users (UMRef group) =
  performReq $ Servant.addUsersToGroup group users

-- | Log-in to UserM using a username and password.
-- Returns a temporary auth token for the logged in user
umLogin :: String -> String -> Cob IO String
umLogin username pass = do
  body <- performReq $ Servant.login (Servant.LoginData username pass)
  parseMaybe (withObject "wO" (.: "securityToken")) body ?? throwError "UserM login response body didn't have securityToken"

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
    tok     <- runCob session (umLogin username pass)
    updateSessionToken session tok


umDeleteUser :: UMRef UMUser -> Cob IO ()
umDeleteUser (UMRef ref) =
  performReq $ Servant.deleteUser ref
