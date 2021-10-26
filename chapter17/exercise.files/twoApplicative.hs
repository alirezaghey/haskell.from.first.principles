#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two x f) (Two y y') = Two (x <> y) (f y')

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary =  Two <$> arbitrary <*> arbitrary

  
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Two  (String, Float, Int) (String, Float, Int))
  quickBatch $ applicative (undefined :: Two String (String, Float, Int))