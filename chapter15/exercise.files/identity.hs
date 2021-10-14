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
    
instance Monoid a =>
  Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

    
instance Arbitrary a =>
  Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)

semigroupAssoc  :: (Eq a, Semigroup a)
                => a -> a -> a -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity  :: (Eq m, Monoid m)
                    => m -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity  :: (Eq m, Monoid m)
                      => m -> Bool
monoidRightIdentity x = x <> mempty == x

type IdenAssoc =
  Identity String -> Identity String -> Identity String -> Bool
  
main :: IO ()
main = do
  let sa  = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck  (sa :: IdenAssoc)
  quickCheck  (mli :: Identity String -> Bool)
  quickCheck  (mri :: Identity String -> Bool)
