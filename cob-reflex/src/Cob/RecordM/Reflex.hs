{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Cob.RecordM.Reflex where

import System.IO

import Data.Time   ( NominalDiffTime, getCurrentTime )
import Control.Monad.Reader

import Reflex.Time ( tickLossyFromPostBuildTime )
import Reflex.Dom 

import Cob
import Cob.RecordM


-- Control

-- | At a refresh rate, and with a query, return the instances in a RecordM definition across all points in time
--
-- If communication with RecordM fails the computation will continue but errors will be printed to stderr
rmDefinitionInstances :: forall a q m t. (MonadWidget t m, RecordMQuery q a)
                      => NominalDiffTime -> q -> CobSession -> m (Dynamic t [a])
rmDefinitionInstances refreshRate query session = do
    now    <- liftIO getCurrentTime
    evTime <- tickLossy refreshRate now
    evt    <- performEvent ((liftIO $ runCobEvent) <$ evTime)
    holdDyn [] evt
    where
        runCobEvent = do
            res <- runCob session (rmDefinitionSearch_ query)
            case res of
              Left err -> hPutStrLn stderr err >> return []
              Right val -> return val

