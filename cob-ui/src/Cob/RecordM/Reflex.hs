{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Cob.RecordM.Reflex
    ( rmDefinitionInstances
    , rmAddInstances
    , NominalDiffTime
    ) where

import System.IO

import Data.Time   ( NominalDiffTime, getCurrentTime )
import Control.Monad.Reader

import Reflex.Dom 

import Cob
import Cob.RecordM


-- Control

-- | At a refresh rate, and with a query, return the instances in a RecordM definition across all points in time
--
-- If communication with RecordM fails the computation will continue but errors will be printed to stderr
rmDefinitionInstances :: forall a q m t. (MonadWidget t m, RecordMQuery q a)
                      => NominalDiffTime
                      -> q -> CobSession -> m (Dynamic t [a])
rmDefinitionInstances refreshRate query session = do
    currentTime <- liftIO getCurrentTime
    evTime <- tickLossy refreshRate currentTime
    pb  <- getPostBuild
    evt <- performEvent ((liftIO runCobEvent) <$ (pb <> (() <$ evTime)))
    holdDyn [] evt
    where
        runCobEvent = do
            res <- runCob session (rmLazyDefinitionSearch_ query)
            case res of
              Left err -> hPutStrLn stderr err >> return []
              Right val -> return val

-- | Add an instance of @a@ to RecordM every time the event of @a@ occurs
rmAddInstances :: forall a q m t. (MonadWidget t m, Record a)
               => Event t a
               -> CobSession
               -> m (Dynamic t [Ref a])
rmAddInstances evt session = do
    addEvt <- performEvent ((liftIO . runCobEvent) <$> evt)
    foldDynMaybe (flip $ \y -> fmap (:y)) [] addEvt
    where
        runCobEvent record = do
            res <- runCob session (rmAddInstance record)
            case res of
              Left err -> hPutStrLn stderr err >> return Nothing
              Right ref -> return (Just ref)

