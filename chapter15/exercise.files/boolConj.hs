#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)
    
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
  
instance Arbitrary BoolConj where
    arbitrary = do
      x <- arbitrary
      return (BoolConj x)

semigroupAssoc  :: BoolConj -> BoolConj -> BoolConj -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: BoolConj -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: BoolConj -> Bool
monoidRightIdentity x = x <> mempty == x

  
main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity

  quickCheck  sa
  quickCheck mli
  quickCheck mri
