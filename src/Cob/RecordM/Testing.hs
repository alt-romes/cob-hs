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

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)

import Data.Bifunctor (first)

import Data.DList (toList)

import Cob
import Cob.RecordM

runRecordMTests :: Monad m => CobSession -> RecordM m a -> m [Int]
runRecordMTests session recm = do
    (res, addedRefs) <- runCobT session recm
    -- liftIO $ forConcurrently_ addedRefs (runCob session) rmDeleteInstance
    return (toList addedRefs)
