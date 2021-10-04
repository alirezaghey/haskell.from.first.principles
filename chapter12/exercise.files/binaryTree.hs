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
    

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0 where
  f x
    | x == n = Nothing
    | otherwise = Just (x+1, x, x+1)