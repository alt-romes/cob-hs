{-# LANGUAGE GADTs #-}
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

import Control.Monad.Free
import Control.Monad.Free.TH

import qualified Streamly.Prelude as Streamly

import Cob.Session (CobToken)
import Cob.RecordM.Query
import Cob.RecordM.Record
import Cob.UserM.Entities
import Cob.Ref

type Cob    = Free (CobF IO)
type CobM m = Free (CobF m)

-- ROMES:TODO: Perhaps use FoldT instead of Serial a -> m b
data CobF m next where
  StreamSearchM :: Record a => Query a -> (Streamly.SerialT m (Ref a, a) -> m b) -> (b -> next) -> CobF m next
  SearchM       :: Record a => Query a -> ([(Ref a, a)] -> next) -> CobF m next
  GetM          :: Record a => Ref a   -> (a -> next) -> CobF m next
  CountM        :: Record a => Query a -> (Int -> next) -> CobF m next
  AddM          :: Record a => a       -> (Ref a -> next) -> CobF m next
  AddSyncM      :: Record a => a       -> (Ref a -> next) -> CobF m next
  DeleteM       :: Record a => Ref a   -> next -> CobF m next
  CreateUserM   :: User       -> (Ref User -> next) -> CobF m next
  DeleteUserM   :: Ref User   -> next -> CobF m next
  AddToGroupM   :: [Ref User] -> Ref Group -> next -> CobF m next
  LoginM        :: String     -> String -> (CobToken -> next) -> CobF m next
  LiftCobM      :: m a -> (a -> next) -> CobF m next

instance Functor (CobF m) where
  fmap g = \case
    StreamSearchM q f h -> StreamSearchM q f (g . h)
    SearchM q f  -> SearchM q (g . f)
    GetM r f     -> GetM r (g . f)
    CountM q f   -> CountM q (g . f)
    AddM q f     -> AddM q (g . f)
    AddSyncM q f -> AddSyncM q (g . f)
    DeleteM r n  -> DeleteM r (g n)
    CreateUserM u f -> CreateUserM u (g . f)
    DeleteUserM u n -> DeleteUserM u (g n)
    AddToGroupM us gr n -> AddToGroupM us gr (g n)
    LoginM u p f -> LoginM u p (g . f)
    LiftCobM x f -> LiftCobM x (g . f)

makeFree_ ''CobF

streamSearch :: Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> Cob b
search       :: Record a => Query a -> Cob [(Ref a, a)]
get          :: Record a => Ref a   -> Cob a
count        :: Record a => Query a -> Cob Int
add          :: Record a => a       -> Cob (Ref a)
addSync      :: Record a => a       -> Cob (Ref a)
delete       :: Record a => Ref a   -> Cob ()
createUser   :: User -> Cob (Ref User)
deleteUser   :: Ref User -> Cob ()
addToGroup   :: [Ref User] -> Ref Group -> Cob ()
login        :: String -> String -> Cob CobToken
liftCob      :: IO a -> Cob a

-- * Versions polymorphic over monad

streamSearchM :: Record a => Query a -> (Streamly.SerialT m (Ref a, a) -> m b) -> CobM m b
searchM       :: Record a => Query a -> CobM m [(Ref a, a)]
getM          :: Record a => Ref a   -> CobM m a
countM        :: Record a => Query a -> CobM m Int
addM          :: Record a => a       -> CobM m (Ref a)
addSyncM      :: Record a => a       -> CobM m (Ref a)
deleteM       :: Record a => Ref a   -> CobM m ()
createUserM   :: User -> CobM m (Ref User)
deleteUserM   :: Ref User -> CobM m ()
addToGroupM   :: [Ref User] -> Ref Group -> CobM m ()
loginM        :: String -> String -> CobM m CobToken
liftCobM      :: m a -> CobM m a



streamSearch = streamSearchM
search       = searchM
get          = getM
count        = countM
add          = addM
addSync      = addSyncM
delete       = deleteM
createUser   = createUserM
deleteUser   = deleteUserM
addToGroup   = addToGroupM
login        = loginM
liftCob      = liftCobM
