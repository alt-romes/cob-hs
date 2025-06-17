{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cob where

import Data.Kind
import Moat (mobileGenWith, prettySwiftData, toMoatData, Options (..), defaultOptions, Protocol (..), ToMoatData)
import Data.Proxy (Proxy (..))
import Cob.RecordM.Definition
import Cob.RecordM.Query

$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''FieldRequired)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''Query)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''Keyword)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''FieldDescription)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''Condition)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''FieldName)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''DefinitionState)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''Field)
$(mobileGenWith defaultOptions { dataProtocols = [Codable] } ''Definition)

generateSwiftCode :: forall (a::Type). ToMoatData a => String
generateSwiftCode = prettySwiftData . toMoatData $ Proxy @a

