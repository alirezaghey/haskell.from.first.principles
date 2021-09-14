module RecSample where

-- Note: These funcs are examples to demonstrate recursion
-- They are in no way optimized

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)


fibonacci :: Integral a => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


type Numerator = Integer 
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

myDivMod :: Numerator -> Denominator -> (Quotient, Remainder)
myDivMod n d = go n d 0
  where go n   d    c
         | n < d = (c, n)
         | otherwise =
           go (n-d) d (c+1)