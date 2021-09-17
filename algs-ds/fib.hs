-- Calcs the greatest num that has a factorial less than
-- or equal to n
-- TC O(m) where m = InvFac(n)
f n = go 1 1 n
  where go c s n
         | s > n = c-1
         | otherwise = go (c+1) (s*(c+1)) n

myFib 1 = 1
myFib 2 = 1
myFib n = go 1 1 n
  where go pp p n
         | n == 3 = pp + p
         | otherwise = go (p) (p+pp) (n-1)

