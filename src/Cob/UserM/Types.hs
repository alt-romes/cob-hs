{-# LANGUAGE OverloadedStrings #-}
module Cob.UserM.Types where

import Data.Maybe
import Data.Aeson

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
