{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cob.UserM where

import Control.Monad.Reader ( ask )
import Control.Monad.IO.Class ( MonadIO )

import Data.Maybe ( fromMaybe )

import Data.Aeson ( Value(..), ToJSON, toJSON, FromJSON, parseJSON, withObject, (.:), object, (.=) )

import Network.HTTP.Simple  ( httpJSONEither, setRequestBodyJSON )
import Network.HTTP.Conduit ( Request(..), Response(..) )

import Cob

-- TODO: How to delete UserM ids to use in tests scenarios ?

type UserM = CobT 'UserM

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
        , "contact"    .= fromMaybe "" pass
        , "usernameAD" .= fromMaybe "" pass ]

data UMGroup = UMGroup

type instance CobWriter 'UserM = ()

-- | Run a UserM computation
runUserM :: Functor m => CobSession -> UserM m a -> m (Either CobError a)
runUserM session = fmap fst . runCobT session
{-# INLINE runUserM #-}


-- | Create an UserM user
umCreateUser :: (Monoid (CobWriter c), MonadIO m) => UMUser -> CobT c m UMRef
umCreateUser user = do
    session <- ask
    let request = setRequestBodyJSON user
                  (cobDefaultRequest session)
                      { method = "POST"
                      , path = "/userm/userm/user" }
    httpValidJSON @UMRef request

umAssignGroups :: (Monoid (CobWriter c), MonadIO m) => UMUser -> [UMGroup] -> CobT c m a
umAssignGroups = undefined

umGenToken :: (Monoid (CobWriter c), MonadIO m) => UMUser -> CobT c m a
umGenToken = undefined
