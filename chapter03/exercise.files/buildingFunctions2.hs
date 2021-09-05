module Chapter03BuildingFunctionsPart02 where

-- solutions to chapter03 exercise "Building functions" part 2

addExclamationToEndOfString :: String -> String
addExclamationToEndOfString text = text ++ "!"

return5thCharOfString :: String -> Char
return5thCharOfString text = text !! 4

returnEverythingAfterThe9thChar :: String -> String
returnEverythingAfterThe9thChar text = drop 9 text 

