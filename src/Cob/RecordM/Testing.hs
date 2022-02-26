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

runRecordMTests :: MonadIO m => CobSession -> RecordM m a -> m (Either CobError a)
runRecordMTests session recm = do
    (res, addedRefs) <- second toList <$> runCobT session recm
    runCobT session (traverse (rmDeleteInstance . Ref) addedRefs)
    return res
