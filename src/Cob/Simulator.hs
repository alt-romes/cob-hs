{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-|
   Simulate the cob backend locally for small interpretation tests. The
   simulation behaves differently from the actual run in many aspects.
   Shouldn't really be used except for demonstrating Haskell free monads.
 -}
module Cob.Simulator where

import qualified Data.Char as C
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Data.Aeson

import qualified Data.IntMap as IM

import Control.Monad.State as State
import Control.Monad.Free

import Unsafe.Coerce

import Cob.RecordM.Query
import Cob.Ref
import Cob

simulate :: Cob a -> IO a
simulate c = evalStateT (foldFree simulateNT c) mempty

newtype SomeRecord = SomeRecord BSL.ByteString -- ^ JSON repr

type SimState = IM.IntMap SomeRecord

simulateNT :: CobF ~> StateT SimState IO
simulateNT = \case
    StreamSearch q f h -> error "Stream search simulator not implemented"
    Search q f  -> do
      hits <- gets IM.toList
      pure $ f (runQuery q hits)
    Get (Ref r) f -> do
      SomeRecord x <- gets (IM.! fromInteger r)
      lift $ print x
      case eitherDecode x of
        Left e -> fail e
        Right y -> pure $ f y
    Count q f   -> do
      hits <- gets IM.toList
      pure $ f $ length $ runQuery q hits
    Add x f     -> do
      r <- State.gets IM.size
      modify (IM.insert r (SomeRecord (BSL.fromStrict . BSC.pack . map C.toLower . BSC.unpack . BSL.toStrict $ encode x)))
      pure (f (Ref . toInteger $ r))
    AddSync x f -> do
      r <- State.gets IM.size
      modify (IM.insert r (SomeRecord (BSL.fromStrict . BSC.pack . map C.toLower . BSC.unpack . BSL.toStrict $ encode x)))
      pure (f (Ref . toInteger $ r))
    Delete (Ref (fromInteger -> r)) n  -> do
      modify' (IM.delete r)
      pure n
    CreateUser _ _ -> error "UserM simulator not implemented"
    DeleteUser _ _ -> error "UserM simulator not implemented"
    AddToGroup {} -> error "UserM simulator not implemented"
    Login {} -> error "UserM simulator not implemented"
    LiftCob i f -> f <$> lift i

runQuery :: FromJSON a => Query a -> [(Int, SomeRecord)] -> [(Ref a, a)]
runQuery q hits = map (\(k, SomeRecord a) -> (Ref (toInteger k), (fromJust . decode) a)) $ filter (\(_, SomeRecord a) -> BSC.isInfixOf (BSC.pack (_q q)) (BSL.toStrict a)) hits
