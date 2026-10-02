{-# LANGUAGE GHC2021, GADTs, LambdaCase, OverloadedStrings, DerivingVia, TypeOperators, RankNTypes, AllowAmbiguousTypes #-}
module Cob.Events
  ( runCobEvents
  , eventM
  ) where

import Control.Concurrent

import Data.Aeson
import Data.String
import Data.Either
import Data.Bifunctor

import qualified Data.List as L

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.RWS.Strict
-- import Control.Monad.Free.Ap
import Control.Monad.Free
import Control.Monad.Free.TH

import Control.Exception
import Cob.Exception
import Cob.Utils (MonadCob)

import qualified Control.Concurrent.Async as A

import qualified Streamly.Data.Stream as Streamly

import qualified Cob.RecordM as RM
import qualified Cob.UserM   as UM

import Cob
import Control.Events

eventM :: ToJSON m => Conn -> String -> EvtMsg m -> (EventId -> Cob (EvtDone, a)) -> Cob a
eventM conn topic msg f = do
  unliftCob $ \unlift ->
    Control.Events.event conn (fromString topic) msg $ \eid -> do
      (done, r) <- unlift (f eid)
      pure (done, r)

-- | Run a cob computation but send events for all destructive operations
--
-- === Example
--
-- @
-- cobAction :: Cob ()
-- cobAction = ...
--
-- main = do
--    session <- UM.umSession ...
--    runCobEvents (Left "server/cob-mimes/...") session cobAction
-- @
runCobEvents :: Conn -> Maybe EventId
             -- ^ A parent scope EventId
             -> CobSession -> Cob ~> IO
runCobEvents conn scopeEv cs cob = unCobEvents (iterM cobRIO cob) (cs, conn)
  where
    cobRIO :: CobF (CobEvents a) -> CobEvents a
    cobRIO = \case
        StreamSearch q f h -> RM.streamDefinitionSearch q (recurse . f . Streamly.morphInner liftCob) >>= h
        Search q f  -> RM.definitionSearch q >>= f
        Get r f     -> RM.getInstance r >>= f
        Count q f   -> RM.definitionCount q >>= f
        Add x f     -> do
          let msg = simple "Create instance" & withMsg ?~ x
          evt "add" msg (\r -> "Created " ++ show r)
            (RM.addInstance x) >>= f
        AddSync x f -> do
          let msg = simple "Create instance (sync)" & withMsg ?~ x
          evt "add-sync" msg (\r -> "Created " ++ show r)
            (RM.addInstanceSync x) >>= f
        Delete r n  -> do
          let msg = simple "Delete instance"
          evt "deleted" msg (\r -> "Deleted " ++ show r)
            (RM.deleteInstance r) >> n
        UpdateInstances q f h -> do
          let msg = simple "Update matching instances" & withMsg ?~ show q
          evt "update" msg (\r -> "Updated " ++ show (map fst r))
            (RM.updateInstances q f) >>= h
        CreateUser u f -> do
          let msg = simple "Create user" & withMsg ?~ u
          evt "create-user" msg (\r -> "Created user " ++ show r)
            (UM.createUser u) >>= f
        DeleteUser u n -> do
          let msg = simple "Delete user" & withMsg ?~ u
          evt "delete-user" msg (\r -> "Deleted user " ++ show r)
            (UM.deleteUser u) >> n
        AddToGroup us gr n -> do
          let msg = simple "Add users to group" & withMsg ?~ (us, gr)
          evt "add-to-group" msg (\r -> "Added users to group " ++ show r)
            (UM.addToGroup us gr) >> n
        Login u p f -> UM.umLogin u p >>= f
        LiftCob x f -> liftIO x >>= f
        UnliftCob x f -> liftIO (x recurse) >>= f
        Try c f     -> liftIO (Control.Exception.try $ recurse c) >>= f
        Catch c h f -> liftIO (Control.Exception.catch (recurse c) (recurse . h)) >>= f
        MapConcurrently h t f -> liftIO (A.mapConcurrently (recurse . h) t) >>= f

    recurse :: Cob ~> IO
    recurse = runCobEvents conn scopeEv cs

    evt :: ToJSON m => String -> EvtMsg m -> (a -> String) -> ReaderT CobSession IO a -> CobEvents a
    evt topic msg succ_msg mc = CobEvents $ \(s, c) -> do
      Control.Events.event c ("cob" <> fromString topic) (msg & scoped .~ scopeEv) $ \_ -> do
        r <- runReaderT mc s
        pure (done (succ_msg r) r)
  

newtype CobEvents a = CobEvents { unCobEvents :: (CobSession, Conn) -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (CobSession, Conn) IO)

instance MonadReader CobSession CobEvents where
  ask = CobEvents $ \(s,_) -> pure s
  local f (CobEvents g) = CobEvents $ \(s, c) -> g (f s, c)

