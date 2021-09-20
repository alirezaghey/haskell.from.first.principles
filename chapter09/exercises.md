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

3. Now let's look at what those two functions have in common. Try writing a new function that parameterizes the character you're breaking the string argument on and rewrite `myWords` and `myLines` using it.

```hs
split :: Char -> String -> [String]
split d s = go d (dropWhile (==d) s) where
  go _ [] = []
  go d s = word : (go d (dropWhile (==d) rest)) where
    (word, rest) = break (==d) s

myWords :: String -> [String]
myWords s = split ' ' s

myLines :: String -> [String]
myLines s = split '\n' s
```
[Solution file](exercise.files/split.hs)

## Comprehend Thy Lists
Take a look at the following functions, figure what you think the output lists will be, and then run them in your REPL to verify (note that you will need the mySqr list from above in scope to do this):
```hs
mySqr = [x^2 | x < [1..10]]

-- What is the output of the following:
[x | x <- mySqr, rem x 2 == 0]
-- Answer
{-
[4, 16, 36, 64, 100]
-}


-- What is the output of the following:
[(x, y) | x <- mySqr,
          y <- mySqr,
          x < 50, y > 50]
-- Answer
{-
[(1, 64), (1, 81), (1, 100),
 (4, 64), (4, 81), (4, 100)
 (9, 64), (9, 81), (9, 100)
 (16, 64), (16, 81), (16, 100)
 (25, 64), (25, 81), (25, 100)
 (36, 64), (36, 81), (36, 100)
 (49, 64), (49, 81), (49, 100)
]
-}


-- What is the output of the following:
take 5 [(x, y) | x <- mySqr,
                 y <- mySqr,
                 x < 50, y > 50]
-- Answer
{-
[(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]
-}


## Square Cube
Given the following:
```REPL
Prelude> mySqr = [x^2 | x <- [1..5]]
Prelude> myCube = [y^3 | x <-[1..5]]
```
1. First write an expression that will make tuples of the outputs of `mySqr` and `myCube`.
```hs
mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
tuples = [(x, y) | x <- mySqr, y <- myCube]
```
