#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) x Nil = x
  (<>) Nil x = x
  (<>) (Cons a as) xs = Cons a (as <> xs)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs <> (<*>) fs xs

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]


main :: IO ()
main = do
  quickBatch $ functor (undefined :: List (Int, Float, String))
  quickBatch $ applicative (undefined :: List (Int, Float, String))