#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z
  -- foldr f ini (Big x y z) = f y $ f z ini

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z



instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Big  (String, Float, Int)
                                            (String, Float, Int))
  quickBatch $ foldable (undefined :: Big (Int, Float, String, Int, String)
                                            (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Big (String, String, String)
                                              (String, String, String))