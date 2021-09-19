# Solutions to problems of chapter 9

## EnumFromTo

Write your own `enumFromTo` definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did `[start..stop]`. Replace the `undefined`, a value which results in an error when evaluated, with your own definitions.

```hs
eftBool :: Bool -> Bool -> [Bool]
eftBool s e
  | s == e    = [s]
  | otherwise = [s, e]

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd s e
  | s == e              = [s]
  | s == LT && e == EQ  = [LT, EQ]
  | s == LT && e == GT  = [LT, EQ, GT]
  | s == EQ && e == GT  = [EQ, GT]
  | otherwise           = []

-- eftOrd using pattern matching
eftOrd2 :: Ordering
        -> Ordering
        -> [Ordering]
eftOrd2 LT EQ = [LT, EQ]
eftOrd2 LT GT = [LT, EQ, GT]
eftOrd2 EQ GT = [EQ, GT]
eftOrd2 s e
  | s == e    = [s]
  | otherwise = []

-- eftOrd using succ
eftOrd3 :: Ordering
        -> Ordering
        -> [Ordering]
eftOrd3 s e
  | s == e    = [s]
  | otherwise = s : eftOrd3 (succ s) e


eftInt :: Int -> Int -> [Int]
eftInt s e
  | s == e    = [s]
  | s < e     = s : eftInt (s+1) e
  | otherwise = []

-- eftInt using succ
eftInt2 :: Int -> Int -> [Int]
eftInt2 s e
  | s == e    = [s]
  | s < e     = s : eftInt2 (succ s) e
  | otherwise = []


eftChar :: Char -> Char -> [Char]
eftChar s e
  | s == e      = [s]
  | s < e       = s : eftChar (succ s) e
  | otherwise   = []
```

[Solution file](exercise.files/enumFromTo.hs)

## Thy Fearful Symmetry

1. Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into works, as in the following sample:
```REPL
Predulde> myWords "sheryl wants fun"
["wallfish", "wants", "fun"]
```
```hs
splitString :: String -> [String]
splitString str = go (' ' : str) 
  where go xs
         | null xs = []
         | otherwise = takeWhile (/=' ') (dropWhile (==' ') xs) : go (dropWhile (/=' ')  (dropWhile (==' ') xs))
```
[Solution file with alternative implementation](exercise.files/splitString.hs)

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string as in the following (your job is to fill in the undefined function):

```hs
module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

-- putStrLn sentences should print
-- Tyger Tyger, burning bright
-- "In the forests of the night
-- What immortal hand or eye
-- "Could frame thy fearful symmetry?"

-- Implement this
myLines :: String -> [String]
myLines = undefined

-- What we want 'myLines sentences' to equal
shouldEqual =
  ["Tyger Tyger, burning bright",
   "In the forests of the night",
   "What immortal hand or eye",
   "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function correctly.
main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
            == shouldEqual)
```
[Solution file](exercise.files/poemLines.hs)