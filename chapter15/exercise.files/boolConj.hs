#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)
    
instance Arbitrary BoolConj where
    arbitrary = do
      x <- arbitrary
      return (BoolConj x)

semigroupAssoc  :: BoolConj -> BoolConj -> BoolConj -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

-- type BoolConjAssoc =
--   BoolConj -> BoolConj -> BoolConj -> Bool
  
main :: IO ()
main = do
  quickCheck  semigroupAssoc
