{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-|
   Simulate the cob backend locally for small interpretation tests?
 -}
module Cob.Simulator where

import qualified Data.IntMap as IM

import Control.Monad.State as State
import Control.Monad.Free
import Unsafe.Coerce

import Cob.RecordM.Record
import Cob.Ref
import Cob

simulate :: Cob a -> a
simulate c = evalState (foldFree simulateNT c) mempty

data SomeRecord where
  SomeRecord :: a -> SomeRecord

type SimState = IM.IntMap SomeRecord

simulateNT :: CobF ~> State SimState
simulateNT = \case
    StreamSearch q f h -> error "Stream search simulator not implemented"
    Search q f  -> error "Search simulator not implemented"
    Get (Ref r) f -> do
      SomeRecord x <- gets (IM.! (fromInteger r))
      pure $ f (unsafeCoerce x)
    Count q f   -> error "Count simulator not implemented"
    Add x f     -> do
      im <- State.get
      let r = IM.size im
      let im' = IM.insert r (SomeRecord x) im
      put im'
      pure (f (Ref . toInteger $ r))
    AddSync x f -> do
      im <- State.get
      let r = IM.size im
      let im' = IM.insert r (SomeRecord x) im
      put im'
      pure (f (Ref . toInteger $ r))
    Delete (Ref (fromInteger -> r)) n  -> do
      modify' (IM.delete r)
      pure n
    CreateUser u f -> error "UserM simulator not implemented"
    DeleteUser u n -> error "UserM simulator not implemented"
    AddToGroup us gr n -> error "UserM simulator not implemented"
    Login u p f -> error "UserM simulator not implemented"
    LiftCob x f -> error "IO simulator not implemented"
