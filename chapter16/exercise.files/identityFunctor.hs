#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck
import Test.QuickCheck.Function


newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

functorIdentity :: --(Functor f, Eq (f a))
                    Identity Int 
                -> Bool
functorIdentity x = fmap id x == x


functorCompose  :: 
                 Fun Int Int
                -> Fun Int Int
                -> Identity Int 
                -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  fmap (g . f) x == fmap g (fmap f x)
  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

main :: IO ()
main = do
  quickCheck functorIdentity
  quickCheck functorCompose
