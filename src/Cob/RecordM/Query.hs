{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|

RecordM 'Query' and combinators to create them.

=== Example

@
let c :: DateTime
    d :: String
    i :: Int

    q =  "date" := c
      <> "desc" := d
      <> "size" := i
... <- search q
@
 -}
module Cob.RecordM.Query where

import Type.Reflection

import Data.String
import qualified Data.Text as T
import Control.Applicative

import Cob.RecordM.DateTime
import Cob.Ref


-- | A @RecordM@ query
--
-- TODO: Remove type parameter?
data Query a = Query { _q         :: String
                     , _from      :: Int
                     , _size      :: Int
                     , _sort      :: Maybe String
                     , _ascending :: Maybe Bool
                     } deriving (Show)

instance IsString (Query a) where
  fromString txt = defaultQuery { _q = fromString txt }

-- | A bit wanky regarding joining sort and ascending
--
-- Joins two query strings '_q' with @AND@, picks the lowest '_from', the
-- largest '_size', and something a bit wanky regarding '_sort' and
-- '_ascending'. When those two are relevant you should override them manually
-- or not use this instance
--
-- TODO: Better '_sort'
instance Semigroup (Query a) where
  q  <> r = Query (_q q <> " AND " <> _q r) (min (_from q) (_from r)) (max (_size q) (_size r)) (max (_sort q) (_sort r)) (liftA2 (&&) (_ascending q) (_ascending r))

-- * Combinators

-- | Create a query with one of the primitive types, or using a type's show instance.
--
-- @
-- let
--    q =  "data_mov"   =: dt
--      <> "descritivo" =: dd
--      <> "movimento"  =: amt
--      <> "saldo"      =: bal
-- @
(=:) :: (Show t, Typeable t) => String -> t -> Query a
s =: t
  | Just HRefl <- typeOf t `eqTypeRep` typeRep @DateTime = fromString $ s <> ":" <> formatTime undefined "%s" t
  | Just HRefl <- typeOf t `eqTypeRep` typeRep @String   = fromString $ s <> ":\"" <> t <> "\""
  | Just HRefl <- typeOf t `eqTypeRep` typeRep @T.Text   = fromString $ s <> ":\"" <> T.unpack t <> "\""
  -- | Just HRefl <- typeOf t `eqTypeRep` typeRep @Int      = fromString $ s <> ":\"" <> show t <> "\""
  -- | Just HRefl <- typeOf t `eqTypeRep` typeRep @Float    = fromString $ s <> ":\"" <> show t <> "\""
  -- | Just HRefl <- typeOf t `eqTypeRep` typeRep @Double   = fromString $ s <> ":\"" <> show t <> "\""
  -- | Just HRefl <- typeOf t `eqTypeRep` typeRep @(Ref a)  = fromString $ s <> ":" <> show t
  | otherwise = fromString $ s <> ":\"" <> show t <> "\""
infix 7 =:

-- * Creation

-- | 'Query' by 'String'
--
-- It's a synonym for @Data.String.fromString@
byStr :: String -> Query a
byStr = fromString

-- | 'Query' by 'Ref'
byRef :: Ref a -> Query a
byRef ref = defaultQuery { _q = "id:" <> show ref }

-- | The default 'Query'
-- @
-- defaultQuery = Query { _q         = "*"
--                      , _from      = 0
--                      , _size      = 5
--                      , _sort      = Nothing
--                      , _ascending = Nothing }
-- @
--
-- It is best used with the record update syntax to construct the desired query.
--
-- ==== __Example__
--
-- @
-- rmDefinitionSearch_ (defaultQuery { _q = "id:123*"
--                                   , _from = 1
--                                   , _size = 21
--                                   , _sort = Just \"id\"
--                                   , _ascending = Just True })
-- @
defaultQuery :: Query a
defaultQuery = Query { _q         = "*"
                     , _from      = 0
                     , _size      = 5
                     , _sort      = Nothing
                     , _ascending = Nothing
                     }
