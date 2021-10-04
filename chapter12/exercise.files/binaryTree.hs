import Data.Maybe (isNothing)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

unfold  :: (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold f x
  | isNothing $ f x   = Leaf
  | otherwise         = Node (unfold f y) z (unfold f k) where
    Just (y, z, k) = f x
