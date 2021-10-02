import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- example input and output for `replaceThe`
-- >>> replaceThe "the cow loves us"
-- "a cow loves us"

-- takes a string and replaces all occrences of 'the'
-- with 'a' ignoring case
-- example: replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words


-- example input and output for `notThe`
-- >>> nothThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

-- takes a string as a word and returns Nothing if it's the
-- otherwise returns Just the inputstring
notThe :: String -> Maybe String
notThe word
  | word == "the" = Nothing
  | otherwise = Just word



-- example input and output for countTheBeforeVowel
-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> CountTheBeforeVowel "the evil cow"
-- 1

-- takes a string and returns the number of "the"s
-- where the next word starts with a vowel
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . foldr go ("", 0) . words where
  go = \word (nextWord, count) -> if not (null nextWord) && toLower (head nextWord) `elem` "aeoiuy" && map toLower word == "the" then (word, count+1) else (word, count)
  

-- same as countTheBeforeVowel
-- explicit recursion
countTheBeforeVowelRec :: String -> Integer 
countTheBeforeVowelRec xs = snd  (go (words xs)) where
  go :: [String] -> (String, Integer)
  go [] = ("", 0)
  go (x:xs) 
   | not (null nextWord) && toLower (head nextWord) `elem` "aeoiuy" && map toLower x == "the" = (x, count+1)
   | otherwise = (x, count)
   where
     (nextWord, count) = go xs