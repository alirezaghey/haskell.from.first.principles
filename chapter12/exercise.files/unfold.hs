-- implementation of the standard iterate function
-- from Data.List
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : go f x [] where
  go f x acc = f x : go f (f x) acc 