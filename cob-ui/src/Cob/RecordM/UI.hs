{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
module Cob.RecordM.UI where

import Data.Text (unpack)

import qualified Cob.RecordM.Reflex as R

import Cob.RecordM
import Cob

import UI.Theme
import UI.Class
import UI

-- Control

definitionInstances :: forall a. Record a
                      => Text -- ^ Query
                      -> CobSession
                      -> UI (Dynamic [a])
definitionInstances q s = UI (R.definitionInstances 30 (byText q) s)

-- | At a refresh rate, and with a query, return the instances in a RecordM definition across all points in time
--
-- If communication with RecordM fails the computation will continue but errors will be printed to stderr
definitionInstances' :: forall a. Record a
                      => R.NominalDiffTime
                      -> Cob.Query a
                      -> CobSession
                      -> UI (Dynamic [a])
definitionInstances' t q s = UI (R.definitionInstances t q s)


-- | Add an instance of @a@ to RecordM every time the event of @a@ occurs
addInstances :: forall a. Record a
               => Event a
               -> CobSession
               -> UI (Dynamic [Ref a])
addInstances e s = UI (R.addInstances e s)

-- | Clickable list with dynamic items fetched from recordm
rmListE :: forall a b. Record a
        => Theme UI
        => Text -- ^ Query -- TODO: Receive cool dynamic text...
        -> CobSession
        -> (Dynamic a -> UI b)
        -> UI (Event b) -- ^ An event with @b@ fired when the list item is clicked
rmListE q s f = do
    vals <- definitionInstances q s
    listE vals f
