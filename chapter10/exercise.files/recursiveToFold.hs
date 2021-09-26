import Distribution.SPDX.LicenseId (LicenseId(XSkat))
-- standard or implementations
-- recursive
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- foldr
myOr2 :: [Bool] -> Bool
myOr2 xs = foldr (||) False xs

-- foldr point-free
myOr3 :: [Bool] -> Bool
myOr3 = foldr (||) False



-- standard any implementations
-- recursive
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []        = False
myAny p (x:xs)    = p x || myAny p xs

-- foldr
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 p xs = foldr (\a b -> p a || b) False xs

-- foldr
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 p xs = foldr (||) False $ map p xs


-- foldr using function composition
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 p xs = foldr ((||) . p) False xs

-- foldr point-free
myAny5 :: (a -> Bool) -> [a] -> Bool
myAny5 p   = foldr ((||) . p) False



-- standard elem implementations
-- recursive
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys)
  | x == y      = True
  | otherwise   = myElem x ys
  

-- foldr
myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 x xs = foldr (\a b -> x == a || b) False xs

-- foldr, point-free
myElem3 :: (Eq a) => a -> [a] -> Bool
myElem3 x = foldr (\a b -> x == a || b) False

-- foldr, map
myElem4 :: (Eq a) => a -> [a] -> Bool 
myElem4 x xs = foldr (||) False $ map (==x) xs 

-- foldr, function composition
myElem5 :: (Eq a) => a -> [a] -> Bool
myElem5 x xs = foldr ((||) . (==x)) False xs

-- foldr, function composition, point-free
myElem6 :: (Eq a) => a -> [a] -> Bool 
myElem6 x = foldr ((||) . (==x)) False

-- using any
myElem7 :: (Eq a) => a -> [a] -> Bool 
myElem7 x = any (== x)


-- standard reverese implementations
-- recursive
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- foldl
myReverse2 :: [a] -> [a]
myReverse2 xs = foldl (\acc x -> x : acc) [] xs

-- foldl without lambda
myReverse3 :: [a] -> [a]
myReverse3 xs = foldl (flip (:)) [] xs

-- foldl, point-free
myReverse4 :: [a] -> [a]
myReverse4 = foldl (flip (:)) []



-- standar map implementations
-- recursive
myMap :: (a -> b) -> [a] -> [b]
myMap _ []      = []
myMap f (x:xs)  = f x : myMap f xs

-- foldr
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 _ []     = []
myMap2 f xs = foldr (\a acc -> (f a) : acc) [] xs

-- foldr, eta-reduced
myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr (\a acc -> (f a) : acc) []

-- foldr, lambda-free
myMap4 :: (a -> b) -> [a] -> [b]
myMap4 f xs = foldr ((:) . f) [] xs

-- foldr, lambda-free, eta-reduced
myMap5 :: (a -> b) -> [a] -> [b]
myMap5 f = foldr ((:) . f) []



-- standard filter implementations
-- recursive
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs) = if p x then x : myFilter p xs else myFilter p xs

-- foldr
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p xs = foldr (\a acc -> if p a then a : acc else acc) [] xs

-- foldr, eta-reduced
myFilter3 :: (a -> Bool) -> [a] -> [a]
myFilter3 p = foldr (\a acc -> if p a then a : acc else acc) []



-- standard concat impplementations
-- recursive
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- foldr
squish2 :: [[a]] -> [a]
squish2 xs = foldr (++) [] xs

-- foldr, point-free
squish3 :: [[a]] -> [a]
squish3 = foldr (++) []




-- standard concatMap impplementations
-- recursive
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- foldr
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f xs = foldr (\a acc -> f a ++ acc) [] xs

-- foldr, eta-reduced
squishMap3 :: (a -> [b]) -> [a] -> [b]
squishMap3 f = foldr (\a acc -> f a ++ acc) []

-- foldr, lambda-free
squishMap4 :: (a -> [b]) -> [a] -> [b]
squishMap4 f xs = foldr ((++) . f) [] xs

-- foldr, lambda-free, eta-reduced
squishMap5 :: (a -> [b]) -> [a] -> [b]
squishMap5 f = foldr ((++) . f) []


-- standard concat implementations relying on concatMap
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\a -> a) xs

-- lambda-free
squishAgain2 :: [[a]] -> [a]
squishAgain2 xs = squishMap id xs

-- lambda-free, eta-reduced
squishAgain3 :: [[a]] -> [a]
squishAgain3 = squishMap id


-- standard maximumBy implementations
-- recursive
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ []    = error "list of length zero"
myMaximumBy f xs     = go f (head xs) xs where
  go f r []         = r
  go f r (x:xs) 
    | f x r == GT   = go f x xs
    | otherwise     = go f r xs 
    
-- foldr
myMaximumBy2 :: (a -> a -> Ordering)
             -> [a] -> a
myMaximumBy2 _ []     = error "list of length zero"
myMaximumBy2 f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs