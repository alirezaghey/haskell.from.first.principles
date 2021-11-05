#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr _ ini (Constant x) = ini

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Constant  String (String, Float, Int))
  quickBatch $ foldable (undefined :: Constant String (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Constant String (String, String, String))