import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord []   = error "empty word"
capitalizeWord word = toUpper (head word) : tail word