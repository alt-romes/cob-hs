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

import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Free.TH

import qualified Streamly.Prelude as Streamly

import Cob.Session (CobToken)
import Cob.RecordM.Query
import Cob.RecordM.Record
import Cob.UserM.Entities
import Cob.Ref

type Cob    = Free CobF

-- ROMES:TODO: Perhaps use FoldT instead of Serial a -> m b
data CobF next where
  StreamSearch :: Record a => Query a -> (Streamly.Serial (Ref a, a) -> IO b) -> (b -> next) -> CobF next
  Search       :: Record a => Query a -> ([(Ref a, a)] -> next) -> CobF next
  Get          :: Record a => Ref a   -> (a -> next) -> CobF next
  Count        :: Record a => Query a -> (Int -> next) -> CobF next
  Add          :: Record a => a       -> (Ref a -> next) -> CobF next
  AddSync      :: Record a => a       -> (Ref a -> next) -> CobF next
  Delete       :: Record a => Ref a   -> next -> CobF next
  CreateUser   :: User       -> (Ref User -> next) -> CobF next
  DeleteUser   :: Ref User   -> next -> CobF next
  AddToGroup   :: [Ref User] -> Ref Group -> next -> CobF next
  Login        :: String     -> String -> (CobToken -> next) -> CobF next
  LiftCob      :: IO a -> (a -> next) -> CobF next

instance Functor CobF where
  fmap g = \case
    StreamSearch q f h -> StreamSearch q f (g . h)
    Search q f  -> Search q (g . f)
    Get r f     -> Get r (g . f)
    Count q f   -> Count q (g . f)
    Add q f     -> Add q (g . f)
    AddSync q f -> AddSync q (g . f)
    Delete r n  -> Delete r (g n)
    CreateUser u f -> CreateUser u (g . f)
    DeleteUser u n -> DeleteUser u (g n)
    AddToGroup us gr n -> AddToGroup us gr (g n)
    Login u p f -> Login u p (g . f)
    LiftCob x f -> LiftCob x (g . f)

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


instance MonadIO Cob where
  liftIO = liftCob

type f ~> g = forall x. f x -> g x
