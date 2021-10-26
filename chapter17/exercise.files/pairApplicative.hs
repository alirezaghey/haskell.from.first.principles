#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')
  

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary =  Pair <$> arbitrary <*> arbitrary


main :: IO ()
main = do
  quickBatch $ functor (undefined :: Pair  (String, Float, Int))
  quickBatch $ applicative (undefined :: Pair (String, Float, Int))