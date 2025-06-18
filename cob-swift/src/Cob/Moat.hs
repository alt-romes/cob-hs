{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Cob.Moat where

import Data.Kind
import Foreign.Swift.Lib
import Cob.RecordM.Definition
import Cob.RecordM.Query

swiftType ''FieldRequired
swiftType ''Query
swiftType ''Keyword
swiftType ''FieldDescription
swiftType ''Condition
swiftType ''FieldName
swiftType ''DefinitionState
swiftType ''Field
swiftType ''Definition
