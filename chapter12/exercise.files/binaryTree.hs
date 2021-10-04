data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

unfold  :: (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold f x = case f x of
  Nothing         -> Leaf
  Just (y, z, k)  -> Node (unfold f y) z (unfold f k)
    

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0 where
  f x
    | x == n = Nothing
    | otherwise = Just (x+1, x, x+1)