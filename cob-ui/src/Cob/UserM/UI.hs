{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob.UserM.UI where

import qualified Cob.UserM.Reflex as R
import Cob

import UI.Class
import UI.Theme
import UI.Layout
import UI.Input
import UI.Text
import UI.Router
import UI

userLogin :: Behavior Text -> Behavior Text -> Event a -> Host -> UI (Event (Maybe CobSession))
userLogin u p ev h = UI $ R.userLogin u p ev h
{-# INLINE userLogin #-}

userLoginPage :: Theme UI => Host -> UI (Event (Maybe CobSession))
userLoginPage host = vstack $ do
    u <- label (text "Username") (input "Username")
    p <- label (text "Password") (inputP "Password")
    click <- button "Login"
    userLogin (current u) (current p) click host

data CobRoute = CRLogin | CRLogout | CRMain

-- | Cob Login + Logout + Main router!
--
-- @
--    cobRouter "*.cultofbits.com" (\session -> do
--
--         mainContent session
--
--         x <- after 150000
--
--         contentView $ hstack $ do
--             toLogout <- button "To logout"
--             toLogin  <- button "To login"
--             return $ leftmost $
--                 [ CRLogout <$ toLogout
--                 , CRLogin  <$ toLogin
--                 , CRLogout <$ x
--                 ]
--                                       )
-- @
cobLogin :: Theme UI => Host -> (CobSession -> UI (Event CobRoute)) -> UI ()
cobLogin host mainContent =
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
            -- TODO: Event or if token time expires
            return ((, Just session) <$> ev)
