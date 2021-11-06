#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
              , checkers

-}
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left x right) = Node (f <$> left) (f x) (f <$> right)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldr _ ini Empty = ini
  foldr f ini (Leaf x) = f x ini
  foldr f ini (Node left x right) = foldr f (f x (foldr f ini right)) left
  -- foldMap _ Empty = mempty
  -- foldMap f (Leaf a) = f a
  -- foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right


instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency
      [ (1, return Empty),
        (2, Leaf <$> arbitrary),
        (2, Node <$> arbitrary <*> arbitrary <*> arbitrary)
        ]

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Tree  (String, Float, Int))
  quickBatch $ foldable (undefined :: Tree (Int, Float, String, Int, String))
  quickBatch $ traversable (undefined :: Tree (String, String, String))