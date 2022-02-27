{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
module Cob.RecordM.Testing where


import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.DList (toList)
import Data.Bifunctor (second)

import Cob
import Cob.RecordM

-- | Run a 'RecordM' computation but all instances added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding and updating test data to RecordM ensuring all added instances are deleted when the test finishes running.
-- Note: updates to already existing instances will NOT be undone.
runRecordMTests :: MonadIO m => CobSession -> RecordM m a -> m (Either CobError a)
runRecordMTests session recm = do
    (res, addedRefs) <- second toList <$> runCobT session recm
    liftIO $ print "Scheduled for deletion:"
    liftIO $ print addedRefs
    (del, _) <- runCobT session (mapM_ (rmDeleteInstance . Ref) addedRefs)
    case del of
      Left err -> return $ Left err
      Right _ -> return res
