{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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

data CobRoute = CRLogin | CRLogout | CRMain

-- | Cob Login + Logout + Main router!
--
--
cobRouter :: Reflex t => Host -> (CobSession -> UI t (Event t CobRoute)) -> UI t ()
cobRouter host mainContent =
    router (CRLogin, Nothing) $ \case

        (CRLogin, Nothing) -> do
            loginEv <- userLoginPage host
            return ((\case
                        Nothing -> (CRLogin, Nothing)
                        Just s -> (CRMain, Just s)) <$> loginEv
                   )

        (CRLogin, Just session) -> do
            text "Already logged in..."
            ((CRMain, Just session) <$) <$> (after 1)

        (CRLogout, _) -> do
            text "Logging out..."
            ((CRLogin, Nothing) <$) <$> (after 1)

        (CRMain, Nothing) -> ((CRLogin, Nothing) <$) <$> verySoon

        (CRMain, Just session) -> do
            ev <- mainContent session
            return ((, Just session) <$> ev)
