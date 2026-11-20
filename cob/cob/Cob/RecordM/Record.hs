{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Cob.RecordM.Record
  ( Record(..)
  , encodeRecord
    -- * Dynamic records
  , DynRecord(..)
  , withDynRecord
  )
  where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding.Internal as AesonEncoding
import Data.Aeson.Key (fromString)
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)

-- | A 'Record' is an instance belonging to a RecordM 'Definition'
-- 
-- The definition name used depends on the type of the 'Record',
-- which will allow the compiler to automatically target the correct definition
-- when generating code to interact with RecordM
--
-- Record types for a definition can be generated automatically using the Cob.RecordM.TH module
--
-- ==== __Example__
--
-- Given a RecordM definition called /Dogs/ with mandatory fields /Name/, /Owner Name/ and /Age/,
-- we could define the following type that instances Record:
--
-- @
-- data DogsRecord = DogsRecord Text Text Int
--
-- instance Record DogsRecord where
--     definition = \"Dogs\"
--     toRecordM (DogsRecord name ownerName age) = object
--         [ \"Name\"       .= name
--         , \"Owner name\" .= ownerName
--         , \"Age\"        .= age ]
--     fromRecordM = withObject "user record" $ \v -> do
--         [name] <- v .: "name"
--         [ownerName] <- v .: "owner_name"
--         [age] <- v .: "age"
--         return (DogsRecord name ownerName (read age))
-- @
class Record a where
    -- | Get the RecordM definition (its name as a string) that has this type of 'Record'
    definition :: String

    -- | Encode the record as a RecordM-compatible JSON for an instance of this
    -- Record's definition
    toRecordM :: a -> Value

    -- | Decode the record from a RecordM-query returning an instance of this
    -- Record's definition.
    fromRecordM :: Value -> Aeson.Parser a

encodeRecord :: Record a => a -> LBS.ByteString
encodeRecord = AesonEncoding.encodingToLazyByteString . AesonEncoding.value . toRecordM

-- | A dynamic instance of 'Record' for which encoding/decoding returns empty.
-- I think the point was to fetch definitions without fixing them at compile
-- time, but TBH I no longer recall!
data DynRecord = DynRecord { dynDefName :: String }

parseDynJSON :: Value -> Aeson.Parser DynRecord
parseDynJSON = withObject "EmptyRecord" $ \v -> do
  DynRecord <$> v .: fromString "definitionName"

-- | Come up with a 'Record' evidence from a runtime 'DynRecord'
-- TODO: Test this in CI (works locally)
withDynRecord :: forall r. DynRecord -> (Record DynRecord => r) -> r
withDynRecord DynRecord{dynDefName} r =
  case unsafeCoerce (FakeDict (DynRecordDict dynDefName (const (object [])) parseDynJSON)) :: Dict (Record DynRecord) of
    Dict -> r

-- | To wrap the runtime dict we unsafe coerce
data FakeDict a = FakeDict a

-- | Very unsafe way to come up with a dictionary at runtime
data DynRecordDict where
    -- | Must mimic 'Record' exactly
    DynRecordDict ::
      { _definition :: String
      , _toRecordM :: DynRecord -> Value
      , _fromRecordM :: Value -> Aeson.Parser DynRecord
      } -> DynRecordDict

-- | Values of type Dict p capture a dictionary for a constraint of type p.
data Dict :: Constraint -> Type where
  Dict :: a => Dict a

