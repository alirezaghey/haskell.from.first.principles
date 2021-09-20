myCompare :: Eq a => [a] -> [a] -> [Bool]
myCompare [] [] = []
myCompare xs [] = replicate (length xs) False 
myCompare [] ys = replicate (length ys) False
myCompare (x:xs) (y:ys) = (x == y) : myCompare xs ys
