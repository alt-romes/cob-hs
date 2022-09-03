{-# LANGUAGE OverloadedStrings #-}
module Cob.Ref where

import Data.Aeson

-- | A 'Ref' of a 'Record' holds the RecordM /id/ of another record
-- 
-- ==== __Example__
--
-- Taking the above /Dogs/ definition,
-- and modifying the /Owner Name/ field to a reference to a different table called /Owners/,
-- the @DogsRecord@ must be modified with `Ref` to reflect this.
--
-- @
-- data OwnersRecord ...
-- data DogsRecord = DogsRecord Text (Ref OwnersRecord) Int
--
-- instance ToJSON DogsRecord where
--     toJSON (DogsRecord name (Ref ownerId) age) = object
--         [ \"Name\" .= name
--         , \"Owner name\" .= show ownerId
--         , \"Age\" .= age ]
-- instance FromJSON DogsRecord where
--     parseJSON = withObject "user record" $ \v -> do
--         [name] <- v .: "name"
--         [ownerId] <- v .: "owner_name"
--         [age] <- v .: "age"
--         return (DogsRecord name (Ref (read ownerId)) (read age))
-- @
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

instance ToJSON (Ref a) where
    toJSON = toJSON . ref_id
    {-# INLINE toJSON #-}

instance FromJSON (Ref a) where
    parseJSON = withObject "Record Id" $ \v -> do
        ref <- v .: "id"
        return (Ref ref)
    {-# INLINE parseJSON #-}


