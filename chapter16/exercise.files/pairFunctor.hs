#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck
import Test.QuickCheck.Function

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

functorIdentity :: Pair Int -> Bool
functorIdentity x = fmap id x == x

functorCompose  :: Fun Int Int
                -> Fun Int Int
                -> Pair Int
                -> Bool
functorCompose (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

main :: IO ()
main = do
  quickCheck functorIdentity
  quickCheck functorCompose