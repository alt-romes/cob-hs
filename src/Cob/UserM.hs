{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Monad.Except ( throwError )
import Control.Monad.Writer ( tell )
import Control.Monad.Reader ( ask )
import Control.Monad.IO.Class ( MonadIO )

import Control.Lens ( (^?) )

import Data.String ( fromString )
import Data.Maybe  ( fromMaybe )
import Data.DList  ( singleton )

import Data.Aeson ( Value(..), ToJSON, toJSON, FromJSON, parseJSON, withObject, (.:), object, (.=) )
import Data.Aeson.Lens ( key, _JSON )

import Network.HTTP.Simple  ( httpJSONEither, setRequestBodyJSON )
import Network.HTTP.Conduit ( Request(..), Response(..) )

import Cob

-- | UserM writes the added users thorought the Cob computation.
-- This allows it to undo (delete) all added users in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runUserMTests', all added instances will be deleted
type instance CobWriter 'UserM = UMRef UMUser

-- | A UserM user id
newtype UMRef a = UMRef Int
instance Show (UMRef a)  where
    show (UMRef i) = show i
    {-# INLINE show #-}
instance ToJSON (UMRef a) where
    toJSON (UMRef i) = toJSON i
    {-# INLINE toJSON #-}
instance FromJSON (UMRef a) where
    parseJSON = withObject "UserM User Id" $ \v -> do
        id <- v .: "id"
        return (UMRef id)
    {-# INLINE parseJSON #-}

-- | An UserM User
data UMUser = UMUser
    { username    :: String
    , password    :: Maybe String
    , name        :: String
    , email       :: String
    , contact     :: Maybe String
    , usernameAD  :: Maybe String
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
umCreateUser :: MonadIO m => UMUser -> Cob m (UMRef UMUser)
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
umAddUsersToGroup :: MonadIO m => [UMRef UMUser] -> UMRef UMGroup -> Cob m ()
umAddUsersToGroup users group = do
    session <- ask
    let request = setRequestBodyJSON users
                  (cobDefaultRequest session)
                      { method = "PUT"
                      , path = "/userm/userm/group/" <> (fromString . show $ group) <> "/users" }
    httpValidNoBody request

-- | Log-in to UserM using a username and password.
-- Returns a temporary auth token for the logged in user
umLogin :: MonadIO m => String -> String -> Cob m String
umLogin username password = do
    session <- ask
    let request = setRequestBodyJSON (object
                    [ "username" .= username
                    , "password" .= password ])
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path = "/userm/security/auth" }
    body <- httpValidJSON @Value request
    body ^? key "securityToken" . _JSON ?? throwError "UserM login response body didn't have securityToken"


umDeleteUser :: MonadIO m => UMRef UMUser -> Cob m ()
umDeleteUser ref = do
    session <- ask
    let request = (cobDefaultRequest session)
                      { method = "DELETE"
                      , path = "/userm/userm/user/" <> fromString (show ref) }
    httpValidNoBody request

