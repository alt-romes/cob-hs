module Cob.RecordM.Query where

import Data.String

-- | A @RecordM@ query
data Query a = Query { _q         :: String
                     , _from      :: Int
                     , _size      :: Int
                     , _sort      :: Maybe String
                     , _ascending :: Maybe Bool
                     } deriving (Show)

instance IsString (Query a) where
  fromString txt = defaultQuery { _q = fromString txt }

-- | 'Query' by 'String'
--
-- It's a synonym for @Data.String.fromString@
byStr :: String -> Query a
byStr = fromString

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
