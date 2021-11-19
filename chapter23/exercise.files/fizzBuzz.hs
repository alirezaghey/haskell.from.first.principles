fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to
  | from > to = []
  | from `mod` 15 == 0 = "fizzbuzz" : fizzbuzzFromTo (from+1) to
  | from `mod` 5 == 0 = "fizz" : fizzbuzzFromTo (from+1) to
  | from `mod` 3 == 0 = "buzz" : fizzbuzzFromTo (from+1) to
  | otherwise = show from : fizzbuzzFromTo (from+1) to