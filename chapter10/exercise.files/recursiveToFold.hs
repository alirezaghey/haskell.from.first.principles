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