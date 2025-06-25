-- SetupHooks.hs
module SetupHooks ( setupHooks ) where

-- Cabal-hooks
import Distribution.Simple.SetupHooks

import Foreign.Swift.SetupHooks

setupHooks :: SetupHooks
setupHooks = foreignSwiftSetupHooks

