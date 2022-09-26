module Cob.Exception where

import Control.Exception

-- | A 'CobException' might be thrown in cob computations due to cob-related
-- errors, such as the creation of a non-unique user.
data CobException
  = NonUniqueUser String -- ^ Creation of a user whose username already exists. It's constructed with the username attempted to create a user.
  deriving Show

instance Exception CobException
