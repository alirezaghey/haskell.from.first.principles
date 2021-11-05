#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ ini Nil = ini
  foldr f ini (Cons x xs) = f x (foldr f ini xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs


instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (5, Cons <$> arbitrary <*> arbitrary)]

main :: IO ()
main = do
  quickBatch $ functor (undefined :: List  (String, Float, Int))
  quickBatch $ foldable (undefined :: List (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: List (String, String, String))