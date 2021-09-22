-- implementing the standard `and` function
-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd xs


-- implementing the standard `or` function
-- direct recursion, not using (||)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) =
  if x then True else myOr xs

-- direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 []      = False
myOr2 (x:xs)  = x || myOr2 xs



-- implementation of standard function `any`
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []      = False
myAny f (x:xs)  = if f x then True else myAny f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ []     = False
myAny2 f (x:xs) = f x || myAny f xs



-- implementation of standard function `elem`
myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem el (x:xs)  = el == x || myElem el xs


myElem2 :: Eq a => a -> [a] -> Bool
myElem2 el = any (== el)



-- implementation of standard `reverse`
myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = myReverse xs ++ [x]


myReverse2 :: [a] -> [a]
myReverse2 []       = []
myReverse2 (x:xs)   = concat [myReverse2 xs, [x]]


-- implementation of the standard `concat` function
squish :: [[a]] -> [a]
squish []             = []
squish (x:xs)         = flatten x where
  flatten []          = squish xs 
  flatten (y:ys)      = y : flatten ys 
  

-- implementation of the standard `concatMap` function
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []        = []
squishMap f (x:xs)    = flatten (f x) where
  flatten []          = squishMap f xs
  flatten (y:ys)      = y : flatten ys


-- implementation of standard function `concat`
-- using `squishMap` from before
squishAgain :: [[a]] -> [a]
squishAgain x = squishMap id x

-- eta reduce and point free version
squishAgain2 :: [[a]] -> [a]
squishAgain2 = squishMap id


-- implementation of standard `maximumBy` function
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ []    = error "list of length zero"
myMaximumBy f x     = go f (head x) x where
  go f r []         = r
  go f r (x:xs) 
    | f x r == GT   = go f x xs
    | otherwise     = go f r xs 
    
-- same as above but with implicit recursion using fold 
myMaximumBy2 :: (a -> a -> Ordering)
             -> [a] -> a
myMaximumBy2 _ []     = error "list of length zero"
myMaximumBy2 f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs



-- implementation of standard minimumBy function
myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ []  = error "list of length zero"
myMinimumBy f x   = go f (head x) x where
  go f r []       = r
  go f r (x:xs)
    | f x r == LT = go f x xs
    | otherwise   = go f r xs

-- same as above but with implicit recursion using fold
myMinimumBy2 :: (a -> a -> Ordering)
             -> [a] -> a
myMinimumBy2 _ []     = error "list of length zero"
myMinimumBy2 f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs
