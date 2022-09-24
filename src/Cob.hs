{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Cob where
  -- (
  --   -- * Operations
  --   streamSearch
  -- , search      
  -- , get         
  -- , count       
  -- , add         
  -- , addSync     
  -- , delete      
  -- , liftCob

  -- -- * Types
  -- , Cob
  -- , CobM
  -- , CobF
  -- ) where

--  romes: add documentation explaining to what functions throw what kind of errors (performRequest throws that servant client error)

import Control.Concurrent

import Data.Bifunctor

import qualified Data.List as L

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.RWS.Strict
-- import Control.Monad.Free.Ap
import Control.Monad.Free
import Control.Monad.Free.TH

import Control.Exception

import qualified Control.Concurrent.Async as A

import qualified Streamly.Prelude as Streamly

import qualified Cob.RecordM as RM
import qualified Cob.UserM   as UM

import Cob.Session
import Cob.RecordM.Query
import Cob.RecordM.Record
import Cob.UserM.Entities
import Cob.Ref

type Cob = Free CobF

data CobF next where
  StreamSearch :: Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> (b -> next) -> CobF next
  Search       :: Record a => Query a -> ([(Ref a, a)] -> next) -> CobF next
  Get          :: Record a => Ref a   -> (a -> next) -> CobF next
  Count        :: Record a => Query a -> (Int -> next) -> CobF next
  Add          :: Record a => a       -> (Ref a -> next) -> CobF next
  AddSync      :: Record a => a       -> (Ref a -> next) -> CobF next
  Delete       :: Record a => Ref a   -> next -> CobF next
  UpdateInstances :: Record a => Query a -> (a -> a) -> ([(Ref a, a)] -> next) -> CobF next
  CreateUser   :: User       -> (Ref User -> next) -> CobF next
  DeleteUser   :: Ref User   -> next -> CobF next
  AddToGroup   :: [Ref User] -> Ref Group -> next -> CobF next
  Login        :: String     -> String -> (CobToken -> next) -> CobF next
  LiftCob      :: IO a -> (a -> next) -> CobF next
  Try          :: Exception e => Cob a -> (Either e a -> next) -> CobF next
  Catch        :: Exception e => Cob a -> (e -> Cob a) -> (a -> next) -> CobF next
  MapConcurrently :: Traversable t => (a -> Cob b) -> t a -> (t b -> next) -> CobF next
  -- NoOp :: next -> CobF next

instance Functor CobF where
  fmap g = \case
    StreamSearch q f h -> StreamSearch q f (g . h)
    Search q f  -> Search q (g . f)
    Get r f     -> Get r (g . f)
    Count q f   -> Count q (g . f)
    Add q f     -> Add q (g . f)
    AddSync q f -> AddSync q (g . f)
    Delete r n  -> Delete r (g n)
    UpdateInstances q f h -> UpdateInstances q f (g . h)
    CreateUser u f -> CreateUser u (g . f)
    DeleteUser u n -> DeleteUser u (g n)
    AddToGroup us gr n -> AddToGroup us gr (g n)
    Login u p f -> Login u p (g . f)
    LiftCob x f -> LiftCob x (g . f)
    Try c f -> Try c (g . f)
    Catch c h f -> Catch c h (g . f)
    MapConcurrently h t f -> MapConcurrently h t (g . f)

-- instance Applicative CobF where
--   pure x = NoOp x
--   f <*> g = undefined -- \case
    -- StreamSearch q f h -> StreamSearch q f (g . h)
    -- Search q f  -> Search q (g . f)
    -- Get r f     -> Get r (g . f)
    -- Count q f   -> Count q (g . f)
    -- Add q f     -> Add q (g . f)
    -- AddSync q f -> AddSync q (g . f)
    -- Delete r n  -> Delete r (g n)
    -- CreateUser u f -> CreateUser u (g . f)
    -- DeleteUser u n -> DeleteUser u (g n)
    -- AddToGroup us gr n -> AddToGroup us gr (g n)
    -- Login u p f -> Login u p (g . f)
    -- LiftCob x f -> LiftCob x (g . f)
    -- Try c f -> Try c (g . f)
    -- Catch c h f -> Catch c h (g . f)
    -- MapConcurrently h t f -> MapConcurrently h t (g . f)

makeFree_ ''CobF

streamSearch :: Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> Cob b
search       :: Record a => Query a -> Cob [(Ref a, a)]
get          :: Record a => Ref a   -> Cob a
count        :: Record a => Query a -> Cob Int
add          :: Record a => a       -> Cob (Ref a)
addSync      :: Record a => a       -> Cob (Ref a)
delete       :: Record a => Ref a   -> Cob ()
updateInstances :: Record a => Query a -> (a -> a) -> Cob [(Ref a, a)]
createUser   :: User -> Cob (Ref User)
deleteUser   :: Ref User -> Cob ()
addToGroup   :: [Ref User] -> Ref Group -> Cob ()
login        :: String -> String -> Cob CobToken
liftCob      :: IO a -> Cob a
try          :: Exception e => Cob a -> Cob (Either e a)
catch        :: Exception e => Cob a -> (e -> Cob a) -> Cob a
mapConcurrently :: Traversable t => (a -> Cob b) -> t a -> Cob (t b)
-- noOp         :: Cob ()


search_ :: Record a => Query a -> Cob [a]
search_ = fmap (map snd) . search

-- ROMES:TODO: retry ?


instance MonadIO Cob where
  liftIO = liftCob

instance MonadFail Cob where
  fail = liftIO . fail

type f ~> g = forall x. f x -> g x
infixr 0 ~>

-- | TODO Catch before returning?
runCob :: CobSession -> Cob a -> IO a
runCob cs = (`runReaderT` cs) . foldFree cobRIO
  where
    cobRIO :: (MonadReader CobSession m, MonadIO m) => CobF ~> m
    cobRIO = \case
        StreamSearch q f h -> h <$> RM.streamDefinitionSearch q f
        Search q f  -> f <$> RM.definitionSearch q
        Get r f     -> f <$> RM.getInstance r
        Count q f   -> f <$> RM.definitionCount q
        Add x f     -> f <$> RM.addInstance x
        AddSync x f -> f <$> RM.addInstanceSync x
        Delete r n  -> n <$  RM.deleteInstance r
        UpdateInstances q f h -> h <$> RM.updateInstances q f
        CreateUser u f -> f <$> UM.createUser u
        DeleteUser u n -> n <$  UM.deleteUser u
        AddToGroup us gr n -> n <$ UM.addToGroup us gr
        Login u p f -> f <$> UM.umLogin u p
        LiftCob x f -> f <$> liftIO x
        Try c f     -> ask >>= \s -> f <$> liftIO (Control.Exception.try $ runCob s c)
        Catch c h f -> ask >>= \s -> f <$> liftIO (Control.Exception.catch (runCob s c) (runCob s . h))
        MapConcurrently h t f -> ask >>= \s -> f <$> liftIO (A.mapConcurrently (runCob s . h) t)

-- | Run a 'Cob' computation but all RecordM instances added and all UserM users added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding test data to RecordM and
-- user data to UserM ensuring all added instances and users are deleted when
-- the test finishes running.
--
-- Note: updates to already existing instances will NOT be undone (for now... could be done with a getRaw and pushRaw of sorts).
--
-- TODO: Could mock to host (mock.example.com)?
mockCob :: CobSession -> Cob a -> IO a
mockCob cs cobf = do

  -- todo: also catch errors when exceptions are thrown in computation... I'll
  -- have to try every individual computation and throw errors with the list?

  (a, (rmRefs, umRefs), ()) <- runRWST (foldFree nt cobf) cs mempty

  threadDelay 2000000

  (`runReaderT` cs) $ do
    mapM_ (RM.deleteInstance . Ref Nothing) rmRefs
    mapM_ (UM.deleteUser . Ref Nothing) umRefs

  pure a

  -- return deletion errors instead of always the result?

  where
    nt :: (MonadState ([Integer], [Integer]) m, MonadReader CobSession m, MonadIO m) => CobF ~> m
    nt = \case
        StreamSearch q f h -> h <$> RM.streamDefinitionSearch q f
        Search q f  -> f <$> RM.definitionSearch q
        Get r f     -> f <$> RM.getInstance r
        Count q f   -> f <$> RM.definitionCount q
        Add x f     -> f <$> do
            r <- RM.addInstance x
            modify' (first (ref_id r:))
            pure r
        AddSync x f -> f <$> do
            r <- RM.addInstanceSync x
            modify' (first (ref_id r:))
            pure r
        Delete r n ->
          -- ROMES:TODO:For-the-others-too: Should I get the instance I'm going to delete first so that I can restore it after?
          n <$ do
            RM.deleteInstance r
            modify' (first (L.delete (ref_id r)))
            pure ()
        UpdateInstances q f h -> h <$> RM.updateInstances q f

        CreateUser u f -> f <$> do
            ur <- UM.createUser u
            modify' (second (ref_id ur:))
            pure ur
        DeleteUser u n ->
          n <$ do
            UM.deleteUser u
            modify' (second (L.delete (ref_id u)))
            pure ()

        AddToGroup us gr n -> n <$ UM.addToGroup us gr
        Login u p f -> f <$> UM.umLogin u p
        LiftCob x f -> f <$> liftIO x
        Try c f     -> ask >>= \s -> f <$> liftIO (Control.Exception.try $ mockCob s c)
        Catch c h f -> ask >>= \s -> f <$> liftIO (Control.Exception.catch (mockCob s c) (mockCob s . h))
        MapConcurrently h t f -> ask >>= \s -> f <$> liftIO (A.mapConcurrently (mockCob s . h) t)

