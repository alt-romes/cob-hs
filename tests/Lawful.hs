{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy

import Data.DList
import Control.Monad.Identity

import Test.QuickCheck
import Test.QuickCheck.Classes

import Cob
import Cob.RecordM
import Cob.UserM

instance Show a => Show (Cob Identity a) where
    show (Cob f) = show (f undefined)

instance Arbitrary a => Arbitrary (DList a) where
    arbitrary = fromList <$> arbitrary

deriving instance Arbitrary (Ref a)
deriving instance Arbitrary (UMRef a)

instance (Monad m, Arbitrary a) => Arbitrary (Cob m a) where
    arbitrary = Cob . const . return <$> arbitrary

instance Eq a => Eq (Cob Identity a) where
    Cob f == Cob g = let f' = f undefined
                         g' = g undefined
                     in f' == g'

main = do
    lawsCheck $ functorLaws cobProxy
    lawsCheck $ applicativeLaws cobProxy
    lawsCheck $ monadLaws cobProxy
    lawsCheck $ alternativeLaws cobProxy

    where cobProxy = Proxy @(Cob Identity)
