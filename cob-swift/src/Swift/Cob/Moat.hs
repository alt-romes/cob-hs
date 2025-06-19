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

-- swiftData ''FieldRequired
-- swiftData ''Query
-- swiftData ''Keyword
-- swiftData ''FieldDescription
-- swiftData ''Condition
-- swiftData ''FieldName
-- swiftData ''DefinitionState
-- swiftData ''Field
-- swiftData ''Definition
--
