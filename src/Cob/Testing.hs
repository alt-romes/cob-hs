{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
module Cob.Testing where


import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.DList (toList)
import Data.Bifunctor (bimap, first, second)

import Cob
import Cob.RecordM
import Cob.UserM

-- | Run a 'Cob' computation but all RecordM instances added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding and updating test data to RecordM ensuring all added instances are deleted when the test finishes running.
-- Note: updates to already existing instances will NOT be undone.
runRecordMTests :: (CobWritersAreMonoids, MonadIO m) => CobSession -> Cob m a -> m (Either CobError a, CobWriter 'UserM)
runRecordMTests session recm = do
    (res, (addedRefs, addedUsers)) <- second (first toList) <$> runCobT session recm
    (del, _) <- runCobT session (mapM_ (rmDeleteInstance . Ref) addedRefs)
    case del of
      Left err -> return (Left err, addedUsers)
      Right _ -> return (res, addedUsers)


-- | Run a 'Cob' computation but all UserM users added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding and updating test data to RecordM ensuring all added instances are deleted when the test finishes running.
-- Note: updates to already existing instances will NOT be undone.
-- runUserMTests :: (CobWritersAreMonoids, MonadIO m) => CobSession -> Cob m a -> m (Either CobError a, CobWriter 'RecordM)
-- runUserMTests session userm = do
--     (res, (addedRefs :: [Int], addedUsers :: [UMRef])) <- second (bimap toList toList) <$> runCobT session userm
--     (del, _) <- runCobT session (mapM_ umDeleteUser addedUsers)
--     case del of
--       Left err -> return (Left err, addedUsers)
--       Right _ -> return (res, addedRefs)
