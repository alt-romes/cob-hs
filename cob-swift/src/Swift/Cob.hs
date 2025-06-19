{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Swift.Cob where

import Cob.UserM
import Cob.RecordM
import Control.Monad.Reader
import Swift.Cob.Moat
import Data.Kind
import Foreign.Swift.Lib
import Cob.RecordM.Definition
import Cob.RecordM.Query
import Cob

-- | Get a definition
cobDefinition :: CobSession -> DefinitionId -> IO Definition
cobDefinition s di = runReaderT (getDefinitionRep di) s

-- | Login to get a CobSession (todo: read the haddocks into Swift too!)
cobLogin :: Host -> String -> String -> IO CobSession
cobLogin a b c = withUMSession a b c pure

-- yieldFunction @(Int -> Query () -> ()) Proxy 'searchCob [Just "session", Just "query"]

-- yieldFunction 'search [Just "session", Just "query"]
-- yieldFunction 'yield
