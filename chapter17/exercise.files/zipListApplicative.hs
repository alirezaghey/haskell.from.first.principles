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

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' i (Cons x xs) = Cons x (take' (i-1) xs)

instance Semigroup (List a) where
  (<>) xs Nil = xs
  (<>) Nil ys = ys
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]


-- ZipList'
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) xs (ZipList' Nil) = xs
  (<>) (ZipList' Nil) ys = ys
  (<>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)

instance Semigroup a => Monoid (ZipList' a) where
  mempty = ZipList' Nil
  mappend = (<>)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' xs where
    xs = Cons x xs
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (go fs xs) where
    go Nil _ = Nil
    go _ Nil = Nil
    go (Cons f fs') (Cons x xs') = Cons (f x) (go fs' xs')
    
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


main :: IO ()
main = do
  putStrLn "List tests"
  quickBatch $ functor (undefined :: List (Int, Float, String))
  quickBatch $ applicative (undefined :: List (Int, Float, String))
  putStrLn "ZipList' test"
  quickBatch $ functor (undefined :: ZipList' (Int, Float, String))
  quickBatch $ applicative (undefined :: ZipList' (Int, Float, String))