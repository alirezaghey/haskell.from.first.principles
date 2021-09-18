splitString :: String -> [String]
splitString str = go (' ' : str) 
  where go xs
         | null xs = []
         | otherwise = takeWhile (/=' ') (dropWhile (==' ') xs) : go (dropWhile (/=' ')  (dropWhile (==' ') xs))

-- alternative solution using `break` and pattern matching
splitString2 :: String -> [String]
splitString2 = go . dropWhile (== ' ')
 where
  go "" = []
  go str = 
     let (nonspace, rest) = break (== ' ') str
     in nonspace : splitString2 rest