module CapitalizeFuncs where

import Data.Char (toUpper, isUpper)

filterCapLetters :: [Char] -> [Char]
filterCapLetters = filter isUpper

capFirstLetter :: [Char] -> [Char]
capFirstLetter []       = []       
capFirstLetter (x:xs)   = toUpper x : xs

-- It would be cleaner using map
-- But we need to implement it recursively for educational purposes
capAllLetters :: [Char] -> [Char]
capAllLetters []      = []
capAllLetters (x:xs)  = toUpper x : capAllLetters xs


getFirstLetterCapitalized :: [Char] -> Char
getFirstLetterCapitalized []      = error "empty string"
getFirstLetterCapitalized (x:xs)  = toUpper x


getFirstCapitalizedComposed :: [Char] -> Char
getFirstCapitalizedComposed []  = error "empty string"
getFirstCapitalizedComposed s   = toUpper . head $ s

getFirstCapitalizedPointFree :: [Char] -> Char
getFirstCapitalizedPointFree    = toUpper . head