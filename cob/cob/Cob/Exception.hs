{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Cob.Exception where

import Cob.Ref

import Control.Exception

-- | A 'CobException' might be thrown in cob computations due to cob-related
-- errors, such as the creation of a non-unique user.
data CobException
  = NonUniqueUser !String -- ^ Creation of a user whose username already exists. It's constructed with the username attempted to create a user.
  | forall a. UnknownUpdateError !(Ref a) -- ^ Error on update of an instance

instance Show CobException where
  show = \case
    NonUniqueUser userName -> "NonUniqueUser Exception: A user whose username (" <> userName <> ") already exists tried to be created using createUser."
    UnknownUpdateError ref -> "UnknownUpdateError Exception: An error was reported when trying to update instance " <> show ref <> ", possibly because an update was done simultaneously. If the direct version were to be used instead of the integration API, this error could be much more precise and we could even have automatic retries when the error is a version conflict."

instance Exception CobException
