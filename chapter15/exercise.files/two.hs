#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b)  =>
  Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) =>
  Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)
    
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)

semigroupAssoc  :: (Eq a, Semigroup a, Eq b, Semigroup b)
                => Two a b -> Two a b -> Two a b -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Eq a, Monoid a, Eq b, Monoid b)
                  =>  Two a b -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Eq a, Monoid a, Eq b, Monoid b)
                  => Two a b -> Bool
monoidRightIdentity x = x <> mempty == x


type TwoAssoc =
  Two String String -> Two String String -> Two String String -> Bool
  
main :: IO ()
main = do
  let sa  = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck  (sa :: TwoAssoc)
  quickCheck  (mli :: Two String String -> Bool)
  quickCheck  (mri :: Two String String -> Bool)

  