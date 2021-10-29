#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Identity  (String, Float, Int)) 
  quickBatch $ applicative (undefined :: Identity  (String, Float, Int))
  quickBatch $ monad (undefined :: Identity  (String, Float, Int))