#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}

import Test.QuickCheck
import Test.QuickCheck.Function

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

functorIdentity :: Three' Int Int-> Bool
functorIdentity x = fmap id x == x

functorCompose  :: Fun Int Int
                -> Fun Int Int
                -> Three' Int Int
                -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  fmap g (fmap f x) == fmap (g . f) x
  

main :: IO ()
main = do
  quickCheck functorIdentity
  quickCheck functorCompose