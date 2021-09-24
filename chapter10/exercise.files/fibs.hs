-- creates an infinite list of fibonacci numbers
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs


-- returns the first 20 fibonacci numbers
fibsFirst20 :: [Integer]
fibsFirst20 = take 20 fibs

-- returns all fibonacci numbers less than 100
fibsLessThan100 :: [Integer]
fibsLessThan100 = takeWhile (<100) fibs


-- creates an infinite list of factorial numbers
fac :: [Integer]
fac = scanl (*) 1 [2..]

-- returns the first n factorial numbers
facN :: Int -> [Integer]
facN n = take n fac

-- pointfree style of the above function
facN2 :: Int -> [Integer]
facN2 = flip take fac
