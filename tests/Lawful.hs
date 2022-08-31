{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy

import System.IO.Unsafe

import Data.DList
import Control.Monad.Identity

import Test.QuickCheck
import Test.QuickCheck.Classes

import Cob
import Cob.RecordM
import Cob.UserM

instance Show a => Show (Cob m a) where
    show _ = "?"

instance Arbitrary a => Arbitrary (DList a) where
    arbitrary = fromList <$> arbitrary

deriving instance Arbitrary (Ref a)
deriving instance Arbitrary (UMRef a)

instance (Monad m, Arbitrary a) => Arbitrary (Cob m a) where
    arbitrary = Cob . const . return <$> arbitrary

instance Eq a => Eq (Cob Identity a) where
    Cob f == Cob g = runIdentity $ do
      (==) <$> (f undefined) <*> (g undefined)

instance Eq a => Eq (Cob IO a) where
    Cob f == Cob g = unsafePerformIO $ do
      (==) <$> (f undefined) <*> (g undefined)

main = do
    lawsCheck $ functorLaws cobProxy
    lawsCheck $ applicativeLaws cobProxy
    lawsCheck $ monadLaws cobProxy
    lawsCheck $ alternativeLaws (Proxy @(Cob IO))

    where cobProxy = Proxy @(Cob Identity)
