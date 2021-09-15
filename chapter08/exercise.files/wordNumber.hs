module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "digitToWord: invalid digit."

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- just for reference a tail recursive version of digits
digitsTailRec :: Integral a => a -> [a]
digitsTailRec n = go n []
  where go n l
         | n < 10    = n : l
         | otherwise = go (n `div` 10) (n `mod` 10 : l)


wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
-- equivalent to intercalate "-" $ map digitToWord $ digits n