#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldr _ ini Nada = ini
  foldr f ini (Yep x) = f x ini

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x


instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Yep <$> arbitrary)]

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Optional  (String, Float, Int))
  quickBatch $ foldable (undefined :: Optional (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Optional (String, String, String))