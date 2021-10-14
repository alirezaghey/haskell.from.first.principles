#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck


data Or a b =
    Fst a
  | Snd a
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b)
      => Semigroup (Or a b) where
  (Snd x)  <> (Snd y) = Snd x
  _        <> (Snd y) = Snd y
  (Snd x)  <> _       = Snd x
  (Fst x)  <> (Fst y) = Fst x


instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc =
      Or String String
  ->  Or String String
  ->  Or String String
  ->  Bool
  
semigroupAssoc :: (Eq m, Semigroup m)
            => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: OrAssoc)