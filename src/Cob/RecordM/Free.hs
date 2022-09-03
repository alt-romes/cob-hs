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

import Control.Monad.IO.Class

import qualified Streamly.Prelude as Streamly

import Cob.RecordM
import qualified Cob

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
    LiftCobM x f -> f <$> liftIO x

