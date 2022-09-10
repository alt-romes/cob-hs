{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cob.UserM.Servant where

import Data.Proxy
import Data.Aeson

import Servant.API
import Servant.Client

import Cob.UserM.Entities
import Cob.Ref

type UserM = "userm" :>

  "userm" :> (
      "user" :> ReqBody '[JSON] User :> Post '[JSON] (Ref User)
      :<|>
      "user" :> Capture "id" Integer :> Delete '[JSON] NoContent
      :<|>
      "group" :> Capture "groupId" Integer :> "users" :> ReqBody '[JSON] [Ref User] :> Put '[JSON] NoContent
              )
  :<|>

  "userm" :> "security" :> "auth" :> ReqBody '[JSON] LoginData :> Post '[JSON] Value

createUser :: User -> ClientM (Ref User)
deleteUser :: Integer -> ClientM NoContent
addUsersToGroup :: Integer -> [Ref User] -> ClientM NoContent
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
