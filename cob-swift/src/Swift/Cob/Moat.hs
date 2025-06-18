{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Swift.Cob.Moat where

import Foreign.Swift.Lib
import Cob.RecordM.Definition
import Cob.RecordM.Query
import Cob.Session
import Foreign

swiftType ''FieldRequired
swiftType ''Query
swiftType ''Keyword
swiftType ''FieldDescription
swiftType ''Condition
swiftType ''FieldName
swiftType ''DefinitionState
swiftType ''Field
swiftType ''Definition

