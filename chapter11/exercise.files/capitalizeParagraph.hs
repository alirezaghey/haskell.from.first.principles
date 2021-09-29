import Data.Char (toUpper)


-- Capitalizes the first leters of each sentence in a paragraph.
-- Relies on the ending '.' to recognize a sentence.
capitalizeParagraph :: String -> String
capitalizeParagraph []    = error "empty text"
capitalizeParagraph text  = unwords $ go $ words $ capitalizeWord text where
  go []         = []
  go [x]     = [x]
  go (x:x':xs)
    | last x == '.' = x: capitalizeWord x' : go xs
    | otherwise     = x : x' : go xs


capitalizeWord :: String -> String
capitalizeWord word = toUpper (head word) : tail word