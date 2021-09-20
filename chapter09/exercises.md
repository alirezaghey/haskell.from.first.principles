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

2. Now alter that expression so that it only uses the `x` and `y` values that are less than 50.

```hs
tuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
```

3. Apply another function to that list comprehension to determine how many tuples inhabit your output list

```hs
length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
```


## Bottom Madness

**Will it blow up?**
Will the following expressions return a value or throw an error?

1. `[x^y | x <- [1..5], y <- [2, undefined]]`
<br>**Answer:** This will blow up. Reason is that the code tries to evaluate undefined.

2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`

<br>**Answer:** This will work. Laziness makes the compiler only evaluate the first element of the list comprehension, avoiding `undefined` altogether.

3. `sum [1, undefined, 3]`
<br>**Answer:** This will blow up. `sum` will force the evaluation of the full list.

4. `length [1, 2, undefined]`

<br>**Answer:** This will work as `length` only evaluate the spine of the list and not the data of the inhabitants of each cell.

5. `length $ [1, 2, 3] ++ undefined`
<br>**Answer:** Blows up since `++` needs to evaluate the right hand operand to make sure it's of the same type as the elements of the list.

6. `take 1 $ filter even [1,2,3, undefined]`
<br>**Answer:** This works since `take 1` will result in only evaluating the first 2 elements of the list.

7. `take 1 $ filter even [1,3,undefined]`
<br>**Answer:** This will blow up since `take 1` forces the right operand of `$` to produce one element. But there are no even elements in the list so it reaches `undefined`.

8. `take 1 $ filter odd [1, 3, undefined]`
<br>**Answer:** This will work.

9. `take 2 $ filter odd [1,3,undefined]`
<br>**Answer:** This will work.

10. `take 3 $ filter odd [1,3,undefined]`
<br>**Answer:** This will blow up.

## Intermission: Is it in normal form?
For each expression below, determine whether it's in:
1. normal form, which implies weak head normal form;
2. weak head normal form only; or,
3. neither

Remember that an expression cannot be in normal form or weak head normal form if the outermost part of the expression isn't a data constructor. It can't be in normal form if any part of the expression is unevaluated.
1. `[1, 2, 3, 4, 5]`
<br>**Answer:** WHNF, NF
2. `1 : 2 : 3 : 4 : _ `
<br>**Answer:** WHNF, but now NF
3. `enumFromTo 1 10`
<br>**Answer:** Neither WHNF, nor NF
4. `lenght [1,2,3,4,5]`
<br>**Answer:** Neither WHNF, nor NF
5. `sum (enumFromTo 1 10)`
<br>**Answer:** Neither WHNF, nor NF
6. `['a'..'m'] ++ ['n'..'z']`
<br>**Answer:** Neither WHNF, nor NF
7. `(_, 'b')`
<br>**Answer:** WHNF, but not NF