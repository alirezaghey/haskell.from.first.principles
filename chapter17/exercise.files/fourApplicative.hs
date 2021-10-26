#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z u) = Four x y z (f u)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four x y z u) (Four x' y' z' u') = Four (x <> x') (y <> y') (z <> z') (u u')

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
                                      => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Four
    (String, Float, Int) (String, Float, Int) (String, Float, Int) (String, Float, Int))
  quickBatch $ applicative (undefined :: Four
    String String String (String, Float, Int))