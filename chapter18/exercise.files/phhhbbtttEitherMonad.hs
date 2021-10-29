#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data PhhhbbtttEither b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Second x) = Second x
  fmap f (First x) = First (f x)

instance Applicative (PhhhbbtttEither b) where
  pure = First
  (Second x) <*> _ = Second x
  _ <*> (Second x) = Second x
  (First f) <*> (First x) = First (f x)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Second x) >>= _ = Second x
  (First x) >>= f = f x

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = frequency [(1, Second <$> arbitrary), (2, First <$> arbitrary)]

  
main :: IO ()
main = do
  quickBatch $ functor (undefined :: PhhhbbtttEither (String, Float, Int)  (String, Float, Int)) 
  quickBatch $ applicative (undefined :: PhhhbbtttEither (String, Float, Int)  (String, Float, Int))
  quickBatch $ monad (undefined :: PhhhbbtttEither (String, Float, Int)  (String, Float, Int))