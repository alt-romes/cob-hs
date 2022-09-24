{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Cob.RecordM.Reflex
    ( definitionInstances
    , addInstances
    , NominalDiffTime
    ) where

import System.IO

import Data.Time   ( NominalDiffTime, getCurrentTime )
import Control.Monad.Reader

import Control.Exception (SomeException)

import qualified Streamly.Prelude as Streamly

import Reflex.Dom 

import Cob


-- Control

-- | At a refresh rate, and with a query, return the instances in a RecordM definition across all points in time
--
-- If communication with RecordM fails the computation will continue but errors will be printed to stderr
definitionInstances :: forall a q m t. (MonadWidget t m, Record a)
                      => NominalDiffTime
                      -> Cob.Query a -> CobSession -> m (Dynamic t [a])
definitionInstances refreshRate query session = do
    currentTime <- liftIO getCurrentTime
    evTime <- tickLossy refreshRate currentTime
    pb  <- getPostBuild
    evt <- performEvent ((liftIO runCobEvent) <$ (pb <> (() <$ evTime)))
    holdDyn [] evt
    where
        runCobEvent = do
            res <- runCob session ((Right . fmap snd <$> streamSearch query Streamly.toList) `catch` \(e :: SomeException) -> pure (Left (show e)))
            case res of
              Left err -> hPutStrLn stderr err >> return []
              Right val -> return val

-- | Add an instance of @a@ to RecordM every time the event of @a@ occurs
addInstances :: forall a q m t. (MonadWidget t m, Record a)
               => Event t a
               -> CobSession
               -> m (Dynamic t [Ref a])
addInstances evt session = do
    addEvt <- performEvent ((liftIO . runCobEvent) <$> evt)
    foldDynMaybe (flip $ \y -> fmap (:y)) [] addEvt
    where
        runCobEvent record = do
            res <- runCob session ((Right <$> add record) `catch` \(e :: SomeException) -> pure (Left (show e)))
            case res of
              Left err -> hPutStrLn stderr err >> return Nothing
              Right ref -> return (Just ref)

