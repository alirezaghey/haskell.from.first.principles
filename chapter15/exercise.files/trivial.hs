#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc  :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc x y z =
  (x <> y) <> z == x <> (y <> z)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: TrivAssoc)