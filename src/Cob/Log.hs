{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-|
   CoB logging capabilities.
-}
module Cob.Log
  ( module Cob.Log
    -- * Fast-logger re-exports
  , toLogStr
  ) where

import Prelude hiding (log)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Cob.Session
import System.Log.FastLogger

logError, logWarn, logInfo, logDebug :: (MonadIO m, MonadReader CobSession m) => LogStr -> m ()
logError = log ERROR
logWarn  = log WARN
logInfo  = log INFO
#ifdef DEBUG
logDebug = log DEBUG
#else
logDebug = pure . const ()
#endif

log :: (MonadIO m, MonadReader CobSession m)
    => Verbosity -> LogStr -> m ()
log logVerbosity str = do
  CobSession{logger, verbosity} <- ask
  liftIO $ 
    case (verbosity, logVerbosity) of
      (DEBUG, _    ) -> logger str
      (INFO,  DEBUG) -> pure ()
      (INFO,  _    ) -> logger str
      (WARN,  DEBUG) -> pure ()
      (WARN,  INFO ) -> pure ()
      (WARN,  _    ) -> logger str
      (ERROR, DEBUG) -> pure ()
      (ERROR, INFO ) -> pure ()
      (ERROR, WARN ) -> pure ()
      (ERROR, ERROR) -> logger str
