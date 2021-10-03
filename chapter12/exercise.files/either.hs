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
  

-- takes a list of Either and
-- returns a list of all the inhabitants of Rights
-- implementation of the standard right function in Data.Either
rights' :: [Either a b] -> [b]
rights' []              = []
rights' ((Right x):xs)  = x : rights' xs
rights' (_:xs)          = rights' xs


-- same as above but using foldr
rightsFoldr' :: [Either a b] -> [b]
rightsFoldr' = foldr f [] where
  f (Right x) acc = x: acc
  f _ acc         = acc
  

-- implementation of the standard function
-- partitionEithers in Data.Either
partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' []                = ([], [])
partitionEithers' (x:xs) = either (\a -> (a:as, bs)) (\b -> (as, b:bs)) x
  where (as, bs) = partitionEithers' xs
  
-- same as above but using foldr
partitionEithersFoldr'  :: [Either a b]
                        -> ([a], [b])
partitionEithersFoldr' = foldr (\x (as, bs) -> either (\a -> (a:as, bs)) (\b -> (as, b:bs)) x) ([],[])

