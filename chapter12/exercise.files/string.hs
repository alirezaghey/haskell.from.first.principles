import Data.Char (toLower)
import Data.Maybe (fromMaybe)


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
