module Subsequence where



isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool 
isSubsequenceOf []            _   = True 
isSubsequenceOf _             []  = False 
isSubsequenceOf xs'@(x:xs) (y:ys)
  | x == y                        = isSubsequenceOf xs  ys
  | otherwise                     = isSubsequenceOf xs' ys
