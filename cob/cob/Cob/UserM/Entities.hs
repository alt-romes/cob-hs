{-# LANGUAGE OverloadedStrings #-}
module Cob.UserM.Entities where

import Data.Maybe
import Data.Aeson

-- | An UserM User
data User = User
    { uusername    :: String
    , upassword    :: Maybe String
    , uname        :: String
    , uemail       :: String
    , ucontact     :: Maybe String
    , uusernameAD  :: Maybe String
    }

instance ToJSON User where
    toJSON (User username pass name email contact usernameAD) = object
        [ "username"   .= username
        , "password"   .= pass
        , "name"       .= name
        , "email"      .= email
        , "contact"    .= fromMaybe "" contact
        , "usernameAD" .= fromMaybe "" usernameAD ]

-- | An UserM Group
data Group = Group
    { gname :: String
    , gdescription :: Maybe String
    }
