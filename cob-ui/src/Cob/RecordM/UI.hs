{-# LANGUAGE ExplicitForAll #-}
module Cob.RecordM.UI where

import qualified Cob.RecordM.Reflex as R

import Reflex (Event)

import Cob.RecordM
import Cob

import UI.Class
import UI

-- Control

rmDefinitionInstances :: forall a q t. RecordMQuery q a
                      => q
                      -> CobSession
                      -> UI t (Dynamic t [a])
rmDefinitionInstances q s = UI (R.rmDefinitionInstances 30 q s)

-- | At a refresh rate, and with a query, return the instances in a RecordM definition across all points in time
--
-- If communication with RecordM fails the computation will continue but errors will be printed to stderr
rmDefinitionInstances' :: forall a q t. RecordMQuery q a
                      => R.NominalDiffTime
                      -> q
                      -> CobSession
                      -> UI t (Dynamic t [a])
rmDefinitionInstances' t q s = UI (R.rmDefinitionInstances t q s)


-- | Add an instance of @a@ to RecordM every time the event of @a@ occurs
rmAddInstances :: forall a t. Record a
               => Event t a
               -> CobSession
               -> UI t (Dynamic t [Ref a])
rmAddInstances e s = UI (R.rmAddInstances e s)
