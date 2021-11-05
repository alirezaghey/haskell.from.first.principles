#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldr f ini (Three x y z) = f z ini

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z


instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Three  (String, Float, Int)
                                            (String, Float, Int)
                                            (String, Float, Int))
  quickBatch $ foldable (undefined :: Three (Int, Float, String, Int, String)
                                            (Int, Float, String, Int, String)
                                            (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Three (String, String, String)
                                              (String, String, String)
                                              (String, String, String))
