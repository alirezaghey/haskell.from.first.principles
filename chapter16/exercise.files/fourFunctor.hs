#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}

import Test.QuickCheck
import Test.QuickCheck.Function

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z u) = Four x y z (f u)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      u <- arbitrary
      return (Four x y z u)

functorIdentity :: Four Int Int Int Int -> Bool
functorIdentity x = fmap id x == x


functorCompose  :: Fun Int Int
                -> Fun Int Int
                -> Four Int Int Int Int
                -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  fmap g (fmap f x) == fmap (g . f) x
  

main :: IO ()
main = do
  quickCheck functorIdentity
  quickCheck functorCompose