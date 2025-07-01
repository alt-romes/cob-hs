{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Cob.RecordM.Record where

import Data.Aeson
import GHC.TypeLits

-- | A 'Record' is an instance belonging to a RecordM 'Definition'
-- 
-- The definition name used depends on the type of the 'Record',
-- which will allow the compiler to automatically target the correct definition
-- when generating code to interact with RecordM
--
-- In the future, record types for each definition could be generated automatically
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

