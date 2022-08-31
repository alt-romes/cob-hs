{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Monad.Except ( throwError )
import Control.Monad.Writer ( tell )
import Control.Monad.Reader ( ask )

import Data.String ( fromString )
import Data.Maybe  ( fromMaybe )
import Data.DList  ( singleton )

import Data.Aeson ( Value(..), ToJSON, toJSON, FromJSON, parseJSON, withObject, (.:), object, (.=) )
import Data.Aeson.Types ( parseMaybe )

import Network.HTTP.Simple  ( setRequestBodyJSON )
import Network.HTTP.Conduit ( Request(..) )

import Cob

-- | UserM writes the added users thorought the Cob computation.
-- This allows it to undo (delete) all added users in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runUserMTests', all added instances will be deleted
type instance CobWriter 'UserM = UMRef UMUser

-- | A UserM user id
newtype UMRef a = UMRef Int deriving (Eq)
instance Show (UMRef a)  where
    show (UMRef i) = show i
    {-# INLINE show #-}
instance ToJSON (UMRef a) where
    toJSON (UMRef i) = toJSON i
    {-# INLINE toJSON #-}
instance FromJSON (UMRef a) where
    parseJSON = withObject "UserM User Id" $ \v -> do
        ref <- v .: "id"
        return (UMRef ref)
    {-# INLINE parseJSON #-}

-- | An UserM User
data UMUser = UMUser
    { uusername    :: String
    , upassword    :: Maybe String
    , uname        :: String
    , uemail       :: String
    , ucontact     :: Maybe String
    , uusernameAD  :: Maybe String
    }
instance ToJSON UMUser where
    toJSON (UMUser username pass name email contact usernameAD) = object
        [ "username"   .= username
        , "password"   .= pass
        , "name"       .= name
        , "email"      .= email
        , "contact"    .= fromMaybe "" contact
        , "usernameAD" .= fromMaybe "" usernameAD ]

-- | An UserM Group
data UMGroup = UMGroup
    { gname :: String
    , gdescription :: Maybe String
    }

-- | Create an UserM user
umCreateUser :: UMUser -> Cob IO (UMRef UMUser)
umCreateUser user = do
    session <- ask
    let request = setRequestBodyJSON user
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path = "/userm/userm/user" }
    ref <- httpValidJSON @(UMRef UMUser) request
    tell (mempty, singleton ref)
    return ref

-- | Add users to a group given their ids
umAddUsersToGroup :: [UMRef UMUser] -> UMRef UMGroup -> Cob IO ()
umAddUsersToGroup users group = do
    session <- ask
    let request = setRequestBodyJSON users
                  (cobDefaultRequest session)
                      { method = "PUT"
                      , path = "/userm/userm/group/" <> (fromString . show $ group) <> "/users" }
    httpValidNoBody request

-- | Log-in to UserM using a username and password.
-- Returns a temporary auth token for the logged in user
umLogin :: String -> String -> Cob IO String
umLogin username pass = do
    session <- ask
    let request = setRequestBodyJSON (object
                    [ "username" .= username
                    , "password" .= pass     ])
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path = "/userm/security/auth" }
    body <- httpValidJSON @Value request
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
    return (updateSessionToken session tok)


umDeleteUser :: UMRef UMUser -> Cob IO ()
umDeleteUser ref = do
    session <- ask
    let request = (cobDefaultRequest session)
                      { method = "DELETE"
                      , path = "/userm/userm/user/" <> fromString (show ref) }
    httpValidNoBody request

