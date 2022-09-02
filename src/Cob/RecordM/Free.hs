{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Cob.RecordM.Free where

import Control.Monad.Free
import Control.Monad.Free.TH

import Control.Monad.Trans
import Control.Monad.IO.Class

import qualified Streamly.Prelude as Streamly

import Cob.RecordM
import qualified Cob

type Cob    = CobM IO
type CobM m = Free (CobF m)

-- ROMES:TODO: Perhaps use FoldT instead of Serial a -> m b
data CobF m next where
  StreamSearchM :: Record a => Query a -> (Streamly.SerialT m (Ref a, a) -> m b) -> (b -> next) -> CobF m next
  SearchM       :: Record a => Query a -> ([(Ref a, a)] -> next) -> CobF m next
  GetM          :: Record a => Ref a   -> (a -> next) -> CobF m next
  CountM        :: Record a => Query a -> (Count a -> next) -> CobF m next
  AddM          :: Record a => a       -> (Ref a -> next) -> CobF m next
  AddSyncM      :: Record a => a       -> (Ref a -> next) -> CobF m next
  DeleteM       :: Record a => Ref a   -> next -> CobF m next
  LiftCob       :: m a -> (a -> next)  -> CobF m next

instance Functor (CobF m) where
  fmap g = \case
    StreamSearchM q f h -> StreamSearchM q f (g . h)
    SearchM q f  -> SearchM q (g . f)
    GetM r f     -> GetM r (g . f)
    CountM q f   -> CountM q (g . f)
    AddM q f     -> AddM q (g . f)
    AddSyncM q f -> AddSyncM q (g . f)
    DeleteM r n  -> DeleteM r (g n)
    LiftCob x f  -> LiftCob x (g . f)

makeFree_ ''CobF

streamSearch :: Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> Cob b
search       :: Record a => Query a -> Cob [(Ref a, a)]
get          :: Record a => Ref a   -> Cob a
count        :: Record a => Query a -> Cob (Count a)
add          :: Record a => a       -> Cob (Ref a)
addSync      :: Record a => a       -> Cob (Ref a)
delete       :: Record a => Ref a   -> Cob ()
streamSearch = streamSearchM
search       = searchM
get          = getM
count        = countM
add          = addM
addSync      = addSyncM
delete       = deleteM
{-# INLINE streamSearch #-}
{-# INLINE search #-}
{-# INLINE get #-}
{-# INLINE count #-}
{-# INLINE add #-}
{-# INLINE addSync #-}
{-# INLINE delete #-}

-- * Versions polymorphic over monad

streamSearchM :: Record a => Query a -> (Streamly.SerialT m (Ref a, a) -> m b) -> CobM m b
searchM       :: Record a => Query a -> CobM m [(Ref a, a)]
getM          :: Record a => Ref a   -> CobM m a
countM        :: Record a => Query a -> CobM m (Count a)
addM          :: Record a => a       -> CobM m (Ref a)
addSyncM      :: Record a => a       -> CobM m (Ref a)
deleteM       :: Record a => Ref a   -> CobM m ()
liftCob       :: m a -> CobM m a

type f ~> g = forall x. f x -> g x
infixr 0 ~>

cob :: Cob.CobSession -> Cob a -> IO a
cob cs = foldFree (natCobIO cs)

natCobIO :: Cob.CobSession -> CobF IO ~> IO
natCobIO cs cobf = Cob.runCob cs $ case cobf of
    StreamSearchM q f h -> h <$> rmStreamDefinitionSearch q f
    SearchM q f  -> f <$> rmDefinitionSearch q
    GetM r f     -> f <$> rmGetInstance r
    CountM q f   -> f <$> rmDefinitionCount q
    AddM x f     -> f <$> rmAddInstance x
    AddSyncM x f -> f <$> rmAddInstanceSync x
    DeleteM r n  -> n <$ rmDeleteInstance r
    LiftCob x f  -> f <$> liftIO x

