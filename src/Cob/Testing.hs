{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
module Cob.Testing where

import Data.DList (toList)
import Data.Bifunctor (bimap, second)

import Cob
import Cob.RecordM
import Cob.UserM

-- | Run a 'Cob' computation but all RecordM instances added and all UserM users added during the computation are removed at the end.
--
-- This is useful in testing purposes: do tests adding and updating test data to
-- RecordM and user data to UserM ensuring all added instances and users are
-- deleted when the test finishes running.
--
-- Note: updates to already existing instances will NOT be undone.
runCobTests :: CobSession -> Cob IO a -> IO a
runCobTests session cob = do
    (res, (addedRefs, addedUsers)) <- second (bimap toList toList) <$> runCobT session cob
    do  -- Delete instances and users and if successful return the original computation result
        _ <- runCob session (mapM_ rmDeleteInstance addedRefs)
        _ <- runCob session (mapM_ umDeleteUser addedUsers)
        -- TODO: return deletion errors instead of always the result
        return res
