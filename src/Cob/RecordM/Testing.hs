{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cob.RecordM.Testing where


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (StateT, modify, runStateT, put)
import Control.Monad.Trans (lift, MonadTrans)

import Cob
import Cob.RecordM

data RecordMOp = NoOp | Search | Count | Add Int {- type unsafe 'Ref' -} | Update | Delete

newtype RecordMTest m a = RecordMTest { unCobTest :: StateT [Int {- type unsafe 'Ref' -}] (CobT m) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadError CobError, MonadReader CobSession)

 
instance MonadTrans RecordMTest where
    lift = RecordMTest . lift . lift

runRecordMTests :: Monad m => RecordMTest m a -> CobT m (a, [Int])
runRecordMTests = flip runStateT [] . unCobTest

rmDefinitionSearchT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> RecordMTest m [(Ref a, a)]
rmDefinitionSearchT = RecordMTest . lift . rmDefinitionSearch

rmDefinitionCountT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> RecordMTest m (Count a)
rmDefinitionCountT = RecordMTest . lift . rmDefinitionCount

rmAddInstanceT :: forall a m. (MonadIO m, Record a) => a -> RecordMTest m (Ref a)
rmAddInstanceT a = do
    ref <- RecordMTest $ lift $ rmAddInstance a
    RecordMTest $ modify (ref_id ref:)
    return ref

rmUpdateInstanceT :: forall a m. (MonadIO m, Record a) => Ref a -> (a -> a) -> RecordMTest m a
rmUpdateInstanceT ref = RecordMTest . lift . rmUpdateInstance ref

rmUpdateInstancesT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> (a -> a) -> RecordMTest m [(Ref a, a)]
rmUpdateInstancesT q = RecordMTest . lift . rmUpdateInstances q

-- TODO: Get or add instance should have add type
