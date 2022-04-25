{-# LANGUAGE OverloadedStrings #-}
module Cob.UserM.UI where

import qualified Cob.UserM.Reflex as R
import Cob

import UI.Extended
import UI

userLogin :: Reflex t => Behavior t Text -> Behavior t Text -> Event t a -> Host -> UI t (Event t (Maybe CobSession))
userLogin u p ev h = UI $ R.userLogin u p ev h
{-# INLINE userLogin #-}

userLoginPage :: Reflex t => Host -> UI t (Event t (Maybe CobSession))
userLoginPage host = contentView $ vstack $ do
    u <- inputL "Username"
    p <- inputL "Password"-- Password
    click <- button "Login"
    userLogin (current u) (current p) click host
