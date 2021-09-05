module Reverse where

-- solutions to chapter03 exercise "Building functions" part 6

rvrs :: String -> String
rvrs text = concat [drop 9 text, take 4 (drop 5 text), take 5 text]

main :: IO ()
main = print $ rvrs "Curry is awesome"
