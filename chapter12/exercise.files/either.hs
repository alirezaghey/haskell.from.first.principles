-- takes a list of Either and
-- returns a list of all the inhabitants of Lefts
-- implementation of the standard left function in Data.Either
lefts' :: [Either a b] -> [a]
lefts' []             = []
lefts' ((Left x):xs)  = x : lefts' xs
lefts' (_:xs)         = lefts' xs


-- same as above but using foldr
leftsFoldr' :: [Either a b] -> [a]
leftsFoldr' = foldr f [] where
  f (Left x) acc  = x : acc
  f _ acc         = acc