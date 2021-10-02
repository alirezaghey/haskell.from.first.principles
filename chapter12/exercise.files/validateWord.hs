newtype Word' =
  Word' String 
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word = if    countConsonants > countVowels
              then  Nothing 
              else  Just (Word' word) where
    (countConsonants, countVowels) =
      foldr f (0, 0) word where
      f x (v, c) = if x `elem` vowels
                   then (v+1, c)
                   else (v, c+1)

            

        