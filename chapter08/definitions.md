# Definitions

1. _Recursion_ is a means of computing results that may require an indefinite amount of work to obtain through the use of repeated function application. Most recursive functions that terminate or otherwise do useful work will often have a case that calls itself and a base case that acts as a backstop of sorts for the recursion.

```hs
-- not recursivbe
lessOne :: Int -> Int
lessOne n = n -1

-- recursive
zero :: Int -> Int
zero 0 = 0
zero n = zero (n - 1)
```
