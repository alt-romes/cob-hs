{-# LANGUAGE OverloadedStrings, DeriveFunctor, RecordWildCards, DataKinds #-}
module Cob.Ref where

import GHC.TypeError
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

-- | A 'Ref' of a 'Record' holds the RecordM /id/ of another record; a 'Ref'
-- can also point to a 'User' in UserM.
newtype Ref a = Ref { ref_id :: Integer } deriving (Eq)

instance Num (Ref a) where
  fromInteger = Ref
  (+) _ _ = error "Don't (+) Refs"
  (*) _ _ = error "Don't (*) Refs"
  (-) _ _ = error "Don't (-) Refs"
  abs _   = error "Don't abs Refs"
  signum _ = error "Don't signum Refs"

instance Show (Ref a) where
    show = show . ref_id
    {-# INLINE show #-}

-- | Encode a 'Ref' directly as an integer in JSON
cobRefValue :: Ref a -> Value
cobRefValue Ref{ref_id} = toJSON ref_id

parseCobRef :: Value -> Aeson.Parser (Ref a)
parseCobRef =
  withObject "Record Id" $ \v -> do
    ref <- v .: "id"
    return (Ref ref)

instance Unsatisfiable
  (Text "A Cob reference is typically written in JSON as an integer, whereas it's parsed from an object from an id key.\
       \ Do not write a ToJSON instance that doesn't roundtrip the FromJSON instance. Use @cobRefValue@ inline.")
  => ToJSON (Ref a) where
  toJSON = unsatisfiable

instance FromJSON (Ref a) where
  parseJSON = parseCobRef

--------------------------------------------------------------------------------

-- | A Cob entity that comes with a reference is typically accompanied by a
-- @version@ as well. The version is required e.g. when updating instances to
-- make sure the version we're updating is the same version the server has
-- upstream (if someone else changed the data on the server in the meantime, an
-- update using an "old" version number would be rejected by the server)
data Versioned a = Versioned { value :: a, version :: !Int }
  deriving Functor

parseVersioned :: (Value -> Aeson.Parser a) -> Value -> Aeson.Parser (Versioned a)
parseVersioned p v = flip (withObject "Versioned") v $ \o -> do
  version <- o .: "version"
  value   <- p (Object o)
  return Versioned{..}
