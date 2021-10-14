#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial
  
instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc  :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc x y z =
  (x <> y) <> z == x <> (y <> z)

monoidLeftIdentity  :: (Eq m, Monoid m)
                    => m -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Eq m, Monoid m)
                    => m -> Bool
monoidRightIdentity x = x <> mempty == x
  
type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)