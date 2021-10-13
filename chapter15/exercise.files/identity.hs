#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a =>
  Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)
    
instance Arbitrary a =>
  Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)

semigroupAssoc  :: (Eq a, Semigroup a)
                => a -> a -> a -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

type IdenAssoc =
  Identity String -> Identity String -> Identity String -> Bool
  
main :: IO ()
main = do
  quickCheck  (semigroupAssoc :: IdenAssoc)
