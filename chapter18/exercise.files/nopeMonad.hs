#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b = Four' a a a b deriving (Eq, Show)


data Nope a = NopeDataJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDataJpg
  
instance Applicative Nope where
  pure x = NopeDataJpg
  _ <*> _ = NopeDataJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDataJpg



instance (Eq a) => EqProp (Nope a) where (=-=) = eq

instance Arbitrary (Nope a) where
    arbitrary = return NopeDataJpg

  
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Nope (String, Float, Int)) 
  quickBatch $ applicative (undefined :: Nope (String, Float, Int))
  quickBatch $ monad (undefined :: Nope (String, Float, Int))