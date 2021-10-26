#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)


-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure x) (Failure y) = Failure (x <> y)
  (<*>) (Failure x) _           = Failure x
  (<*>) _           (Failure y) = Failure y
  (<*>) (Success f) (Success y) = Success (f y)
  

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Failure <$> arbitrary), (2, Success <$> arbitrary)]


main :: IO ()
main = do
  quickBatch $ functor (undefined :: Validation String (String, String, String))
  quickBatch $ applicative (undefined :: Validation String (String, String, String))