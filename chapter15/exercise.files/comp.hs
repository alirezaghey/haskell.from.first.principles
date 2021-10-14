#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck

newtype Combine a b =
  Combine {unCombine :: a -> b}
  
-- newtype Combine2 a b =
--   Combine2 (a -> b)
  
-- unCombine :: Combine2 a b -> (a->b)
-- unCombine (Combine2 f) = f
  
-- data Or a b =
--     Fst a
--   | Snd a
--   deriving (Eq, Show)

instance (Semigroup a, Semigroup b)
      => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance Show (Combine a b) where
  show (Combine _) = "Combine a b"

instance (coArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

type CombineAssoc =
      Combine (const String)
  ->  Combine (const String)
  ->  Combine (const String)
  ->  Bool
  
semigroupAssoc :: (Eq m, Semigroup m)
            => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: CombineAssoc)