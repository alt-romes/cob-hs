{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Cob
  ( module Cob
  , module Cob.Session
  , module Cob.Exception
  , module Cob.Ref
  , module Cob.RecordM.Record
  , module Cob.RecordM.Query
  , module Cob.UserM.Entities

  -- Cob.RecordM re-exported
  , FileUpload(..)
  ) where

import Control.Concurrent

import Cob.Exception
import Cob.RecordM (FileUpload(..))
import Cob.RecordM.Query
import Cob.RecordM.Record
import Cob.Ref
import Cob.Session
import Cob.UserM.Entities
import Cob.Utils (MonadCob)
import Control.Exception
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.Either (lefts)
import Data.IORef
import qualified Cob.RecordM as RM
import qualified Cob.UserM   as UM
import qualified Control.Concurrent.Async as A
import qualified Data.List as L
import qualified Streamly.Data.Stream as Streamly

type Cob = Free CobF

data CobF next where
  StreamSearch :: Record a => Query a -> (Streamly.Stream Cob (Ref a, a) -> Cob b) -> (b -> next) -> CobF next
  Search       :: Record a => Query a -> ([(Ref a, a)] -> next) -> CobF next
  Get          :: Record a => Ref a   -> (a -> next) -> CobF next
  Count        :: Record a => Query a -> (Int -> next) -> CobF next
  Add          :: Record a => a       -> (Ref a -> next) -> CobF next
  AddSync      :: Record a => a       -> (Ref a -> next) -> CobF next
  Delete       :: Record a => Ref a   -> next -> CobF next
  UpdateInstances :: Record a => Query a -> (a -> a) -> ([(Ref a, a)] -> next) -> CobF next
  UploadFile   :: Ref a -> String -> FileUpload -> next -> CobF next
  CreateUser   :: User       -> (Ref User -> next) -> CobF next
  DeleteUser   :: Ref User   -> next -> CobF next
  AddToGroup   :: [Ref User] -> Ref Group -> next -> CobF next
  Login        :: String     -> String -> (CobToken -> next) -> CobF next

  -- Control flow
  LiftCob      :: IO a -> (a -> next) -> CobF next
  UnliftCob    :: ((forall a. Cob a -> IO a) -> IO b) -> (b -> next) -> CobF next
  Try          :: Exception e => Cob a -> (Either e a -> next) -> CobF next
  Catch        :: Exception e => Cob a -> (e -> Cob a) -> (a -> next) -> CobF next
  MapConcurrently :: Traversable t => (a -> Cob b) -> t a -> (t b -> next) -> CobF next

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
    UploadFile r f c k -> UploadFile r f c (g k)
    CreateUser u f -> CreateUser u (g . f)
    DeleteUser u n -> DeleteUser u (g n)
    AddToGroup us gr n -> AddToGroup us gr (g n)
    Login u p f -> Login u p (g . f)
    LiftCob x f -> LiftCob x (g . f)
    UnliftCob x f -> UnliftCob x (g . f)
    Try c f -> Try c (g . f)
    Catch c h f -> Catch c h (g . f)
    MapConcurrently h t f -> MapConcurrently h t (g . f)

makeFree_ ''CobF

streamSearch :: Record a => Query a -> (Streamly.Stream Cob (Ref a, a) -> Cob b) -> Cob b
search       :: Record a => Query a -> Cob [(Ref a, a)]
get          :: Record a => Ref a   -> Cob a
count        :: Record a => Query a -> Cob Int
add          :: Record a => a       -> Cob (Ref a)
addSync      :: Record a => a       -> Cob (Ref a)
delete       :: Record a => Ref a   -> Cob ()
updateInstances :: Record a => Query a -> (a -> a) -> Cob [(Ref a, a)]
uploadFile   :: Ref a -> String -> FileUpload -> Cob ()
createUser   :: User -> Cob (Ref User)
deleteUser   :: Ref User -> Cob ()
addToGroup   :: [Ref User] -> Ref Group -> Cob ()
login        :: String -> String -> Cob CobToken
liftCob      :: IO a -> Cob a
unliftCob    :: ((forall a. Cob a -> IO a) -> IO b) -> Cob b
try          :: Exception e => Cob a -> Cob (Either e a)
catch        :: Exception e => Cob a -> (e -> Cob a) -> Cob a
mapConcurrently :: Traversable t => (a -> Cob b) -> t a -> Cob (t b)

search_ :: Record a => Query a -> Cob [a]
search_ = fmap (map snd) . search

instance MonadIO   Cob where liftIO = liftCob
instance MonadFail Cob where fail = liftIO . fail

type f ~> g = forall x. f x -> g x
infixr 0 ~>

-- | Run a cob computation
--
-- === Example
--
-- @
-- cobAction :: Cob ()
-- cobAction = ...
--
-- main = do
--    session <- UM.umSession ...
--    runCob session cobAction
-- @
runCob :: CobSession -> Cob ~> IO
runCob cs = (`runReaderT` cs) . foldFree cobRIO
  where
    cobRIO :: MonadCob m => CobF ~> m
    cobRIO = \case
        StreamSearch q f h -> h <$> RM.streamDefinitionSearch q (recurse . f . Streamly.morphInner liftCob)
        Search q f  -> f <$> RM.definitionSearch q
        Get r f     -> f <$> RM.getInstance r
        Count q f   -> f <$> RM.definitionCount q
        Add x f     -> f <$> RM.addInstance x
        AddSync x f -> f <$> RM.addInstanceSync x
        Delete r n  -> n <$  RM.deleteInstance r
        UpdateInstances q f h -> h <$> RM.updateInstances q f
        UploadFile r f c k -> k <$ RM.uploadFile r f c
        CreateUser u f -> f <$> UM.createUser u
        DeleteUser u n -> n <$  UM.deleteUser u
        AddToGroup us gr n -> n <$ UM.addToGroup us gr
        Login u p f -> f <$> UM.umLogin u p
        LiftCob x f -> f <$> liftIO x
        UnliftCob x f -> f <$> liftIO (x recurse)
        Try c f     -> f <$> liftIO (Control.Exception.try $ recurse c)
        Catch c h f -> f <$> liftIO (Control.Exception.catch (recurse c) (recurse . h))
        MapConcurrently h t f -> f <$> liftIO (A.mapConcurrently (recurse . h) t)

    recurse :: Cob ~> IO
    recurse = runCob cs

-- | Run a 'Cob' computation but all RecordM instances added and all UserM users added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding test data to RecordM and
-- user data to UserM ensuring all added instances and users are deleted when
-- the test finishes running.
--
-- Cleanup runs even if the computation throws.
--
-- /WARNING/: updates and deletes to already existing instances will NOT be undone
-- (for now... could be done with a getRaw and pushRaw of sorts).
--
-- TODO: Could mock to host (mock.example.com)?
mockCob :: Int -- ^ Delay in seconds
        -> CobSession -> Cob ~> IO
mockCob delayInSeconds cs cobf = do
  refs <- newIORef ([], [])
  runTracked refs cobf `finally` do
    threadDelay (delayInSeconds * 1000000)
    (rmRefs, umRefs) <- readIORef refs
    results <- mapM (Control.Exception.try @SomeException . (`runReaderT` cs)) $
      map (RM.deleteInstance . Ref Nothing) rmRefs ++ map (UM.deleteUser . Ref Nothing) umRefs
    mapM_ throwIO (take 1 (lefts results))
  where
    -- Nested computations share the same 'IORef', so cleanup happens only once, at the end.
    runTracked :: IORef ([Integer], [Integer]) -> Cob ~> IO
    runTracked refs = (`runReaderT` cs) . foldFree nt
      where
        track :: MonadIO m => (([Integer], [Integer]) -> ([Integer], [Integer])) -> m ()
        track f = liftIO (atomicModifyIORef' refs (\s -> (f s, ())))

        nt :: MonadCob m => CobF ~> m
        nt = \case
            StreamSearch q f h -> h <$> RM.streamDefinitionSearch q (recurse . f . Streamly.morphInner liftCob)
            Search q f  -> f <$> RM.definitionSearch q
            Get r f     -> f <$> RM.getInstance r
            Count q f   -> f <$> RM.definitionCount q
            Add x f     -> f <$> do
                r <- RM.addInstance x
                track (first (ref_id r:))
                pure r
            AddSync x f -> f <$> do
                r <- RM.addInstanceSync x
                track (first (ref_id r:))
                pure r
            Delete r n ->
              -- ROMES:TODO:For-the-others-too: Should I get the instance I'm going to delete first so that I can restore it after?
              n <$ do
                RM.deleteInstance r
                track (first (L.delete (ref_id r)))
            UpdateInstances q f h -> h <$> RM.updateInstances q f
            UploadFile r f c k -> k <$ RM.uploadFile r f c

            CreateUser u f -> f <$> do
                ur <- UM.createUser u
                track (second (ref_id ur:))
                pure ur
            DeleteUser u n ->
              n <$ do
                UM.deleteUser u
                track (second (L.delete (ref_id u)))

            AddToGroup us gr n -> n <$ UM.addToGroup us gr
            Login u p f -> f <$> UM.umLogin u p
            LiftCob x f -> f <$> liftIO x
            UnliftCob x f -> f <$> liftIO (x recurse)
            Try c f     -> f <$> liftIO (Control.Exception.try $ recurse c)
            Catch c h f -> f <$> liftIO (Control.Exception.catch (recurse c) (recurse . h))
            MapConcurrently h t f -> f <$> liftIO (A.mapConcurrently (recurse . h) t)

        recurse :: Cob ~> IO
        recurse = runTracked refs

