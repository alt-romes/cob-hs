{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Monad.Writer ( tell )
import Control.Monad.Reader ( ask )
import Control.Monad.IO.Class ( MonadIO )

import Data.String ( fromString )
import Data.Maybe  ( fromMaybe )
import Data.DList  ( singleton )

import Data.Aeson ( Value(..), ToJSON, toJSON, FromJSON, parseJSON, withObject, (.:), object, (.=) )

import Network.HTTP.Simple  ( httpJSONEither, setRequestBodyJSON )
import Network.HTTP.Conduit ( Request(..), Response(..) )

import Cob

-- | UserM writes the added users thorought the Cob computation.
-- This allows it to undo (delete) all added users in a computation (particularly in a testing environment)
--
-- This list of added instances is **unused** when the computation is run with 'runCob'.
-- However, when the computation is run with 'runUserMTests', all added instances will be deleted
type instance CobWriter 'UserM = UMRef

-- | A UserM user id
newtype UMRef = UMRef Int
instance Show UMRef where
    show (UMRef i) = show i
    {-# INLINE show #-}
instance ToJSON UMRef where
    toJSON (UMRef i) = toJSON i
    {-# INLINE toJSON #-}
instance FromJSON UMRef where
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

data UMGroup = UMGroup

-- | Create an UserM user
umCreateUser :: MonadIO m => UMUser -> Cob m UMRef
umCreateUser user = do
    session <- ask
    let request = setRequestBodyJSON user
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path = "/userm/userm/user" }
    ref <- httpValidJSON @UMRef request
    tell (mempty, singleton ref)
    return ref

umAssignGroups :: MonadIO m => UMUser -> [UMGroup] -> Cob m a
umAssignGroups = undefined

umGenToken :: MonadIO m => UMUser -> Cob m a
umGenToken = undefined

umDeleteUser :: MonadIO m => UMRef -> Cob m ()
umDeleteUser ref = do
    session <- ask
    let request = (cobDefaultRequest session)
                      { method = "DELETE"
                      , path = "/userm/userm/user/" <> fromString (show ref) }
    httpValidNoBody request

