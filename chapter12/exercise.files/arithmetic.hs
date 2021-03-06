import Data.List


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n + x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0


mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1


mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go acc [] = acc
        go acc (x:xs) = go (acc ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []


