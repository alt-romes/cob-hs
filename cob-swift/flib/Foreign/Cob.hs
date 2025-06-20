{-# OPTIONS_GHC -Wno-orphans -ddump-splices -ddump-simpl #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin Foreign.Swift.Lib #-}
module Foreign.Cob where

import Cob.UserM
import Cob.RecordM
import Cob.Session
import Control.Monad.Reader

import Foreign.Swift
import Foreign.Swift.Marshal
import Foreign.Swift.Lib

import Cob.RecordM.Definition
import Cob.RecordM.Query

import Data.Proxy (Proxy(..))
import qualified Data.Kind as K

-- Yield types to library
-- yieldType @FieldRequired Proxy
-- yieldType @(Query _) Proxy
-- yieldType @Keyword Proxy
-- yieldType @FieldDescription Proxy
-- yieldType @Condition Proxy
-- yieldType @FieldName Proxy
-- yieldType @DefinitionState Proxy
-- yieldType @Field Proxy
-- yieldType @Definition Proxy

swiftData ''FieldRequired
swiftData ''Query
swiftData ''Keyword
swiftData ''FieldDescription
swiftData ''Condition
swiftData ''FieldName
swiftData ''DefinitionState
swiftData ''Field
swiftData ''Definition
swiftData ''DefinitionId

swiftPtr ''CobSession

swiftMarshal PtrKind ''CobSession
swiftMarshal JSONKind ''DefinitionId
swiftMarshal JSONKind ''Definition

-- | Get a definition
cobDefinition :: CobSession -> DefinitionId -> IO Definition
cobDefinition s di = runReaderT (getDefinitionRep di) s
$(foreignExportSwift 'cobDefinition)

-- | Login to get a CobSession (todo: read the haddocks into Swift too!)
cobLogin :: Host -> String -> String -> IO CobSession
cobLogin a b c = withUMSession a b c pure
$(foreignExportSwift 'cobLogin)

