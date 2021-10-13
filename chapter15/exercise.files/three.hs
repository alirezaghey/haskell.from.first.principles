#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)  =>
  Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three x y z)

semigroupAssoc  :: (Eq a, Semigroup a, Eq b, Semigroup b, Eq c, Semigroup c)
                => Three a b c -> Three a b c -> Three a b c -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z


type ThreeAssoc =
  Three String String String -> Three String String String -> Three String String String -> Bool
  
main :: IO ()
main = do
  quickCheck  (semigroupAssoc :: ThreeAssoc)

  