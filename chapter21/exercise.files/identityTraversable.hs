#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  
instance Foldable Identity where
  foldr f ini (Identity x) = f x ini

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x


instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Identity  (String, Float, Int))
  quickBatch $ foldable (undefined :: Identity (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Identity (String, String, String))