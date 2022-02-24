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

import Cob
import Cob.RecordM

-- runCobTestsT :: Monad m => CobSession -> CobT m a -> m (Either CobError a)
-- runCobTestsT session cob = runReaderT (runExceptT (fst <$> runWriterT (unCob cob))) session

-- runCobTests :: CobSession -> Cob a -> IO (Either CobError a)
-- runCobTests session cob = do
--     let (x, w) = runWriterT (unCob cob)
--     runReaderT (runExceptT x) session
