#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck
import Test.QuickCheck.Function

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)


functorIdentity :: Two Int Int -> Bool
functorIdentity x = fmap id x == x

functorCompose  :: Fun Int Int
                -> Fun Int Int
                -> Two Int Int
                -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  fmap g (fmap f x) == fmap (g . f) x

main :: IO ()
main = do
  quickCheck functorIdentity
  quickCheck functorCompose