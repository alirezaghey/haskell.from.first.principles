#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}
import Test.QuickCheck (arbitrary, Arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> xs = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)

instance Monad List where
  return x = Cons x Nil 
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)
  
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch $ functor (undefined :: List (Int, Float, String))
  quickBatch $ applicative (undefined :: List (Int, Float, String))
  quickBatch $ monad (undefined :: List (Int, Float, String))