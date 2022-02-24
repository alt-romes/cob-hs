{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
module Cob.RecordM.Testing where


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (StateT, modify, runStateT, put)
import Control.Monad.Trans (lift, MonadTrans)

import Cob
import Cob.RecordM

data RecordMOp = NoOp | Search | Count | Add Int {- UNSAFE 'Ref' -} | Update | Delete

data RecordMTest m a = RecordMTest { operation :: RecordMOp
                                   , unCobTest :: StateT [Int {- UNSAFE 'Ref' -}] (CobT m) a }
 

instance Functor f => Functor (RecordMTest f) where
    fmap f (RecordMTest op x) = RecordMTest op (fmap f x)

instance Monad m => Applicative (RecordMTest m) where
    pure  = RecordMTest NoOp . pure
    (RecordMTest (Add ref) f) <*> (RecordMTest op x) = RecordMTest op (modify (ref:) >> f <*> x)
    (RecordMTest _ f) <*> (RecordMTest op x) = RecordMTest op (f <*> x)

instance Monad m => Monad (RecordMTest m) where
    (RecordMTest (Add ref) x) >>= f = do
        session <- RecordMTest NoOp $ lift ask
        errorOrX <- lift $ runCobT session $ runStateT x []
        case errorOrX of
          Left err -> RecordMTest NoOp $ lift $ throwError err
          Right (x', s) -> RecordMTest NoOp (modify (ref:)) >> f x'

    (RecordMTest _ x) >>= f = do
        session <- RecordMTest NoOp $ lift ask
        errorOrX <- lift $ runCobT session $ runStateT x []
        case errorOrX of
          Left err -> RecordMTest NoOp $ lift $ throwError err
          Right (x', s) -> f x'

instance MonadTrans RecordMTest where
    lift = RecordMTest NoOp . lift . lift

runRecordMTests :: Monad m => RecordMTest m a -> CobT m (a, [Int])
runRecordMTests = flip runStateT [] . unCobTest

rmDefinitionSearchT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> RecordMTest m [(Ref a, a)]
rmDefinitionSearchT = RecordMTest Search . lift . rmDefinitionSearch

rmDefinitionCountT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> RecordMTest m (Count a)
rmDefinitionCountT = RecordMTest Cob.RecordM.Testing.Count . lift . rmDefinitionCount

rmAddInstanceT :: forall a m. (MonadIO m, Record a) => a -> RecordMTest m (Ref a)
rmAddInstanceT a =
    RecordMTest NoOp (lift (rmAddInstance a)) >>= \ref ->
        RecordMTest (Add (ref_id ref)) (return ref)
    -- RecordMTest (Add 5) (lift (rmAddInstance a))

rmUpdateInstanceT :: forall a m. (MonadIO m, Record a) => Ref a -> (a -> a) -> RecordMTest m a
rmUpdateInstanceT ref = RecordMTest Update . lift . rmUpdateInstance ref

rmUpdateInstancesT :: forall a m q. (MonadIO m, Record a, RecordMQuery q a) => q -> (a -> a) -> RecordMTest m [(Ref a, a)]
rmUpdateInstancesT q = RecordMTest Update . lift . rmUpdateInstances q

-- TODO: Get or add instance should have add type
