{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Cob where

import Cob.Moat
import Data.Kind
import Foreign.Swift.Lib
import Cob.RecordM.Definition
import Cob.RecordM.Query

yieldType @FieldRequired Proxy
yieldType @(Query ()) Proxy
yieldType @Keyword Proxy
yieldType @FieldDescription Proxy
yieldType @Condition Proxy
yieldType @FieldName Proxy
yieldType @DefinitionState Proxy
yieldType @Field Proxy
yieldType @Definition Proxy

-- streamSearch :: Record a => Query a -> (Streamly.Stream IO (Ref a, a) -> IO b) -> Cob b
-- search       :: Record a => Query a -> Cob [(Ref a, a)]
-- get          :: Record a => Ref a   -> Cob a
-- count        :: Record a => Query a -> Cob Int
-- add          :: Record a => a       -> Cob (Ref a)
-- addSync      :: Record a => a       -> Cob (Ref a)
-- delete       :: Record a => Ref a   -> Cob ()
-- updateInstances :: Record a => Query a -> (a -> a) -> Cob [(Ref a, a)]
-- createUser   :: User -> Cob (Ref User)
-- deleteUser   :: Ref User -> Cob ()
-- addToGroup   :: [Ref User] -> Ref Group -> Cob ()
-- login        :: String -> String -> Cob CobToken
-- liftCob      :: IO a -> Cob a
-- try          :: Exception e => Cob a -> Cob (Either e a)
-- catch        :: Exception e => Cob a -> (e -> Cob a) -> Cob a
-- mapConcurrently :: Traversable t => (a -> Cob b) -> t a -> Cob (t b)
