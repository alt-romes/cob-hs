{-# LANGUAGE MonoLocalBinds #-}
module Cob.RecordM.Reflex where

import Data.Time   ( NominalDiffTime, getCurrentTime )
import Control.Monad.Reader

import Reflex.Time ( tickLossyFromPostBuildTime )
import Reflex.Dom 

import Cob
import Cob.RecordM

-- | With a session, a refresh rate, and a query, return the instances in a RecordM definition across all points in time
rmDefinitionInstances :: (RecordMQuery q a, MonadWidget t m)
                      => CobSession -> NominalDiffTime -> q -> m (Dynamic t (Either CobError [a]))
rmDefinitionInstances session refreshRate query = do
    now    <- liftIO getCurrentTime
    evTime <- tickLossy refreshRate now
    evt    <- performEvent ((liftIO $ runCob session (rmDefinitionSearch_ query)) <$ evTime)
    holdDyn (Right []) evt
