module Chapter03BuildingFunctionsPart05 where

-- solutions to chapter03 exercise "Building functions" part 5

rvrs :: String -> String
rvrs text = concat [drop 9 text, take 4 (drop 5 text), take 5 text]
