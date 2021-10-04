-- implementation of the standard iterate function
-- from Data.List
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : go f x [] where
  go f x acc = f x : go f (f x) acc 
  

-- implementation of the standard unfoldr function
-- from Data.List
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing           -> []
  Just (a, b)       -> a : myUnfoldr f b


-- implementation of the standar iterate function
-- using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr g x where
  g x = Just (x, f x)