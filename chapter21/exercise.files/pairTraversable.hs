#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldr f ini (Pair x y) = f y ini

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y


instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Pair  (String, Float, Int)
                                            (String, Float, Int))
  quickBatch $ foldable (undefined :: Pair (Int, Float, String, Int, String)
                                            (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Pair (String, String, String)
                                              (String, String, String))