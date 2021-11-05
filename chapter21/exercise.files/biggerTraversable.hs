#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger x y z u) = Bigger x (f y) (f z) (f u)

instance Foldable (Bigger a) where
  foldMap f (Bigger x y z u) = f y <> f z <> f u
  -- foldr f ini (Bigger x y z u) = f y $ f z $ f u ini

instance Traversable (Bigger a) where
  traverse f (Bigger x y z u) = Bigger x <$> f y <*> f z <*> f u
  

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Bigger  (String, Float, Int)
                                            (String, Float, Int))
  quickBatch $ foldable (undefined :: Bigger (Int, Float, String, Int, String)
                                            (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Bigger (String, String, String)
                                              (String, String, String))