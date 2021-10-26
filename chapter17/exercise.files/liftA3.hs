import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: String -> String -> String -> [(Char, Char, Char)]
combos = liftA3 (,,)
