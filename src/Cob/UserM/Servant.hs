{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cob.UserM.Servant where

import Data.Proxy

import Servant.API
import Servant.Client
import Data.Aeson

import Cob.UserM.Types

type UserM = "userm" :>

  "userm" :> (
      "user" :> ReqBody '[JSON] UMUser :> Post '[JSON] (UMRef UMUser)
      :<|>
      "user" :> Capture "id" Int :> Delete '[JSON] ()
      :<|>
      "group" :> Capture "groupId" Int :> "users" :> ReqBody '[JSON] [UMRef UMUser] :> Put '[JSON] ()
              )
  :<|>

  "security" :> "auth" :> ReqBody '[JSON] LoginData :> Post '[JSON] Value

createUser :: UMUser -> ClientM (UMRef UMUser)
deleteUser :: Int -> ClientM ()
addUsersToGroup :: Int -> [UMRef UMUser] -> ClientM ()
login :: LoginData -> ClientM Value
(createUser :<|> deleteUser :<|> addUsersToGroup) :<|> login = client (Proxy @UserM)


data LoginData
  = LoginData
      String -- ^ Username
      String -- ^ Password
instance ToJSON LoginData where
  toJSON (LoginData username pass) = object
    [ "username" .= username
    , "password" .= pass     ]
