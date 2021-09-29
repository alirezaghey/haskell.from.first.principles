module CapitalizeWords where

import Data.Char


capitalizeWords :: String
                -> [(String, String)]
capitalizeWords text = map capitalize $ myWords text where
  capitalize []         = error "empty string"
  capitalize w@(w':ws)  = (w,toUpper w' : ws )


split :: Char -> String -> [String]
split d s = go d (dropWhile (==d) s) where
  go _ [] = []
  go d s = word : go d (dropWhile (==d) rest) where
    (word, rest) = break (==d) s

myWords :: String -> [String]
myWords = split ' '