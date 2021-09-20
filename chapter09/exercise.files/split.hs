module Split where

split :: Char -> String -> [String]
split d s = go d (dropWhile (==d) s) where
  go _ [] = []
  go d s = word : go d (dropWhile (==d) rest) where
    (word, rest) = break (==d) s

myWords :: String -> [String]
myWords = split ' '

myLines :: String -> [String]
myLines = split '\n'