{-# LANGUAGE GHC2021, GADTs, LambdaCase, OverloadedStrings, DerivingVia, TypeOperators, RankNTypes, AllowAmbiguousTypes #-}
module Cob.Events
  ( runCobEvents
  , eventM

    -- * Re-exports
  , module Control.Events
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

eventM :: ToJSON m => Conn -> EvtMsg m -> String -> (EventId -> Cob (EvtDone, a)) -> Cob a
eventM conn msg topic f = do
  unliftCob $ \unlift ->
    Control.Events.event conn msg (fromString topic) $ \eid -> do
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
runCobEvents :: Either String EventId
             -- ^ Base topic or a parent scope EventId
             -> CobSession -> Cob ~> IO
runCobEvents base cs cob =
    withConn (either fromString (const "") base) $ \conn ->
     unCobEvents (iterM cobRIO cob) (cs, conn)
  where
    cobRIO :: CobF (CobEvents a) -> CobEvents a
    cobRIO = \case
        StreamSearch q f h -> RM.streamDefinitionSearch q (recurse . f . Streamly.morphInner liftCob) >>= h
        Search q f  -> RM.definitionSearch q >>= f
        Get r f     -> RM.getInstance r >>= f
        Count q f   -> RM.definitionCount q >>= f
        Add x f     -> do
          let msg = simple "Create instance" & withMsg ?~ x
          evt msg "add" (\r -> "Created " ++ show r)
            (RM.addInstance x) >>= f
        AddSync x f -> do
          let msg = simple "Create instance (sync)" & withMsg ?~ x
          evt msg "add-sync" (\r -> "Created " ++ show r)
            (RM.addInstanceSync x) >>= f
        Delete r n  -> do
          let msg = simple "Delete instance"
          evt msg "deleted" (\r -> "Deleted " ++ show r)
            (RM.deleteInstance r) >> n
        UpdateInstances q f h -> do
          let msg = simple "Update matching instances" & withMsg ?~ show q
          evt msg "update" (\r -> "Updated " ++ show (map fst r))
            (RM.updateInstances q f) >>= h
        CreateUser u f -> do
          let msg = simple "Create user" & withMsg ?~ u
          evt msg "create-user" (\r -> "Created user " ++ show r)
            (UM.createUser u) >>= f
        DeleteUser u n -> do
          let msg = simple "Delete user" & withMsg ?~ u
          evt msg "delete-user" (\r -> "Deleted user " ++ show r)
            (UM.deleteUser u) >> n
        AddToGroup us gr n -> do
          let msg = simple "Add users to group" & withMsg ?~ (us, gr)
          evt msg "add-to-group" (\r -> "Added users to group " ++ show r)
            (UM.addToGroup us gr) >> n
        Login u p f -> UM.umLogin u p >>= f
        LiftCob x f -> liftIO x >>= f
        UnliftCob x f -> liftIO (x recurse) >>= f
        Try c f     -> liftIO (Control.Exception.try $ recurse c) >>= f
        Catch c h f -> liftIO (Control.Exception.catch (recurse c) (recurse . h)) >>= f
        MapConcurrently h t f -> liftIO (A.mapConcurrently (recurse . h) t) >>= f

    recurse :: Cob ~> IO
    recurse = runCobEvents base cs

    evt :: ToJSON m => EvtMsg m -> String -> (a -> String) -> ReaderT CobSession IO a -> CobEvents a
    evt msg topic succ_msg mc = CobEvents $ \(s, c) -> do
      Control.Events.event c (msg & scoped .~ parent) ("cob" <> fromString topic) $ \_ -> do
        r <- runReaderT mc s
        pure (done (succ_msg r) r)

    parent :: Maybe EventId
    parent = either (const Nothing) Just base
  

newtype CobEvents a = CobEvents { unCobEvents :: (CobSession, Conn) -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (CobSession, Conn) IO)

instance MonadReader CobSession CobEvents where
  ask = CobEvents $ \(s,_) -> pure s
  local f (CobEvents g) = CobEvents $ \(s, c) -> g (f s, c)

