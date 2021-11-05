#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers

-}

{-# LANGUAGE FlexibleContexts #-}

-- module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n  => Functor (S n) where
  fmap f (S fx y) = S (f <$> fx) (f y) 

instance Foldable n => Foldable (S n) where
  foldr f ini (S fx y) = foldr f (f y ini) fx


instance Traversable n => Traversable (S n) where
  traverse f (S tx y)= S <$> traverse f tx <*> f y


instance (Functor n
        , Arbitrary (n a)
        , Arbitrary a )
      => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n
        , Testable (n Property)
        , Eq a
        , Eq (n a)
        , EqProp a)
      =>  EqProp (S n a) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch $ functor (undefined :: S [] (Int, Float, String))
  quickBatch $ foldable (undefined :: S [] (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: S [] (Int, Float, String))