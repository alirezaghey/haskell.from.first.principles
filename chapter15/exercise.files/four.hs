#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)  =>
  Semigroup (Four a b c d) where
    (Four x y z u) <> (Four x' y' z' u') = Four (x <> x') (y <> y') (z <> z') (u <> u')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      u <- arbitrary
      return (Four x y z u)

semigroupAssoc  :: (Eq a, Semigroup a, Eq b, Semigroup b,
                    Eq c, Semigroup c, Eq d, Semigroup d)
                => Four a b c d -> Four a b c d -> Four a b c d -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z


type FourAssoc =
  Four String String String String ->
  Four String String String String ->
  Four String String String String ->
  Bool
  
main :: IO ()
main = do
  quickCheck  (semigroupAssoc :: FourAssoc)

  