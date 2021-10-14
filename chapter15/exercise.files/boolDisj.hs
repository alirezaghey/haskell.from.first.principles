#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)
    
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

  
instance Arbitrary BoolDisj where
    arbitrary = do
      x <- arbitrary
      return (BoolDisj x)

semigroupAssoc  :: BoolDisj -> BoolDisj -> BoolDisj -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: BoolDisj -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: BoolDisj -> Bool
monoidRightIdentity x = x <> mempty == x

  
main :: IO ()
main = do
  quickCheck  semigroupAssoc
  quickCheck  monoidLeftIdentity
  quickCheck  monoidRightIdentity