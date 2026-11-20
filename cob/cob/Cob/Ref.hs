{-# LANGUAGE OverloadedStrings #-}
module Cob.Ref where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson

-- | A 'Ref' of a 'Record' holds the RecordM /id/ of another record
data Ref a = Ref { ref_version :: !(Maybe Integer), ref_id :: !Integer } deriving (Eq)

{-
Note [Cob references]
~~~~~~~~~~~~~~~~~~~~~
A cob reference is an integer that points to a Record or User.
Besides the integer, a reference may also hold the current version of the referenced instance.
This version is necessary to guarantee that updates are not overriding unseen changes (caused by concurrent updates by multiple users) in 'updateInstances'

The main situations that determine whether the reference will have a version:

  * When calling a function such as 'definitionSearch', where multiple
  hits/instances will be returned for a query, each individual record instance
  will have an id and an associated version. 'addInstance's return a version = 0
  reference to the newly created record.

  * However, a record with fields that contain references to other records will
  not contain anything but the id (i.e. no version!)

  * Constructing a reference manually (eg using 'Num') will not give it a version

In practice, this is perfectly fine. For creating queries and fetching records the version is unused.
The version is only required when updating instances, but that is currently
only possible via 'updateInstances' which takes a Query as an input, not a
Reference (so, eg, if you updateInstances by query id =. my_ref, it will fetch
the id with the version before proceding -- we always need to fetch to have a
base record to update with the function). If we ever want to have a direct
update, it will mostly be fine as well because, to have access to any record to
update, you must have fetched it -- which always brings the version along. We'd
certainly want some kind of runtime validation in that hypothetical function
though.
-}

instance Num (Ref a) where
  fromInteger = Ref Nothing
  (+) _ _ = error "Don't (+) Refs"
  (*) _ _ = error "Don't (*) Refs"
  (-) _ _ = error "Don't (-) Refs"
  abs _   = error "Don't abs Refs"
  signum _ = error "Don't signum Refs"

instance Show (Ref a) where
    show = show . ref_id
    {-# INLINE show #-}

writeCobRef :: Ref a -> Value
writeCobRef Ref{ref_id} = toJSON ref_id

parseCobRef :: Value -> Aeson.Parser (Ref a)
parseCobRef = withObject "Record Id" $ \v -> do
  ref <- v .: "id"
  version <- v .: "version"
  return (Ref version ref)

