-- SetupHooks.hs
module SetupHooks ( setupHooks ) where

-- Cabal-hooks
import Distribution.Simple.SetupHooks

setupHooks :: SetupHooks
setupHooks = noSetupHooks

