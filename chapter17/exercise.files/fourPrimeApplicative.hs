#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z u) = Four' x y z (f u)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' x y z u) (Four' x' y' z' u') = Four' (x <> x') (y <> y') (z <> z') (u u')

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Four' (String, Float, Int) (String, Float, Int)) 
  quickBatch $ applicative (undefined :: Four' String (String, Float, Int))