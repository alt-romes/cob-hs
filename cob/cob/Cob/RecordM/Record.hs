{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Cob.RecordM.Record
  ( Record(..)
    -- * Dynamic records
  , DynRecord(..)
  , withDynRecord
  )
  where

import Data.Aeson
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
-- instance ToJSON DogsRecord where
--     toJSON (DogsRecord name ownerName age) = object
--         [ \"Name\"       .= name
--         , \"Owner name\" .= ownerName
--         , \"Age\"        .= age ]
-- instance FromJSON DogsRecord where
--     parseJSON = withObject "user record" $ \v -> do
--         [name] <- v .: "name"
--         [ownerName] <- v .: "owner_name"
--         [age] <- v .: "age"
--         return (DogsRecord name ownerName (read age))
--
-- instance Record DogsRecord where
--     definition = \"Dogs\"
-- @
class (ToJSON a, FromJSON a) => Record a where
    -- | Get the RecordM definition (its name as a string) that has this type of 'Record'
    definition :: String

-- | A dynamic instance of 'Record' for which ToJSON/FromJSON gets no record fields at all
data DynRecord = DynRecord { dynDefName :: String }

instance ToJSON DynRecord where
  toJSON _ = object []

instance FromJSON DynRecord where
  parseJSON = withObject "EmptyRecord" $ \v -> do
    DynRecord <$> v .: fromString "definitionName"

-- | Come up with a 'Record' evidence from a runtime 'DynRecord'
-- TODO: Test this in CI (works locally)
withDynRecord :: forall r. DynRecord -> (Record DynRecord => r) -> r
withDynRecord DynRecord{dynDefName} r =
  case unsafeCoerce (FakeDict (DynRecordDict dynDefName)) :: Dict (Record DynRecord) of
    Dict -> r

-- | To wrap the runtime dict we unsafe coerce
data FakeDict a = FakeDict a

-- | Very unsafe way to come up with a dictionary at runtime
data DynRecordDict where
    -- | Must mimic 'Record' exactly
    DynRecordDict :: (ToJSON DynRecord, FromJSON DynRecord) => {
        _definition :: String
    } -> DynRecordDict

-- | Values of type Dict p capture a dictionary for a constraint of type p.
data Dict :: Constraint -> Type where
  Dict :: a => Dict a

