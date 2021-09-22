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

## More Bottoms

As always, we encourage you to ry figuring out the answers before you enter them into your REPL.

1. Will the following expression return a value or throw an error?
<br>`take 1 $ map (+1) [undefined, 2, 3]`
<br>**Answer:** Will throw an error. Reason is that `take` needs one element and the first element to `map` is `undefined`.

2. Will the following expression return a value?
<br> `take 1 $ map (+1) [1, undefined, 3]`
<br>**Answer:** This will work and produce `[2]`.

3. Will the following expression return a value?
<br>`take 2 $ map (+1) [1, undefined, 3]`
<br>**Answer:** No. This will throw an erro.

4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to  make sure you we correct.
```hs
itIsMystery xs =
  map (\x -> elem x "aeiou") xs
```
**Answer:** The effective type of the above function is: `[Char] -> [Bool]`. It will go over every character of the input string `xs` and if its in `aeiou` (case sensitie), outputs `True` in its stead, otherwise `False`.

For instance `itIsMystery "I am learning Haskell"` would result in `[False, False, True, False, False, False, True, True, False, False, True, False, False, False, False, True, False, False, True, False, False]`.

5. What will be the result of the following functions:
<br> a. `map (^2) [1..10]`
<br>**Answer:** `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
<br>b. `map minimum [[1..10], [10..20], [20..30]]`
<br>**Answer:** `[1, 10, 20]`
<br>c. `map sum [[1..5], [1..5], [1..5]]`
<br>**Answer:** `[15, 15, 15]`

6. Back in chapter 7, you wrote a function called `foldBool`. That function exists in a module known as `Data.Bool` and is called `bool`. Write a function that does the same (or similar, if you with) as the `map (if-then-else)` function you saw above but uses `bool` instead of the `if-then-else` syntax. Your first step should be bringing the `bool` function into scope by typing `import Data.Bool` at your Prelude prompt.

```hs
import Data.Bool

fIfElse :: Integral a => [a] -> [a]
fIfElse = map (\x -> if x == 3 then (-x) else x)

-- above function using bool
fBool :: Integral a => [a] -> [a]
fBool = map (\x -> bool x (-x) (x==3))
```
[Solution file](exercise.files/ifElseBool.hs)


## Filtering

1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?

```hs
filter (\x -> rem x 3 == 0) [1..30]
```

2. Recalling what we learned about function composition, how could we compose the above function with the `length` function to tell us **how many** multiples of `3` there are between `1` and `30`?

```hs
length $ filter (\x -> rem x 3 == 0) [1..30]
```

3. Next we're going to work on removing all articles ('the', 'a', and 'an') from sentences. You want to get to something that works like this:

```REPL
Prelude> myFilter "the brown dog was a goof"
["brown", "dog", "was", "goof"]
```
```hs
removeArticles s = filter (\x -> isArticle x) $ words s
  where isArticle word =  word /= "a" &&
                          word /= "A" &&
                          word /= "an" &&
                          word /= "An" &&
                          word /= "the" &&
                          word /= "The"

removeArticles2 s = filter (\x -> isArticle x) $ words s
  where isArticle word = notElem word ["a", "A", "an", "An", "the", "The"]
  
removeArticles3 s = filter isArticle $ words s
  where isArticle word = notElem word ["a", "A", "an", "An", "the", "The"]
  
removeArticles4 s = filter (\x -> notElem x ["a", "A", "an", "An", "the", "The"]) $ words s

removeArticles5 = filter (\x -> notElem x ["a", "A", "an", "An", "the", "The"]) . words

removeArticles6 = filter (`notElem` ["a", "A", "an", "An", "the", "The"]) . words

removeArticles7 = filter ((`notElem` ["a", "an", "the"]) . map toLower) . words
```

## Zipping exercise
1. Write your own version of `zip` and ensure it behaves the same as the original.

```hs
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
```
[Solution file](exercise.files/myZip.hs)

2. Do what you did for `zip`, but now for `zipWith`:

```hs
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _   []        = []
myZipWith _ []  _         = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
```
[Solution file](exercise.files/myZipWith.hs)


3. Rewrite your `zip` in terms of the `zipWith` you wrote.

```hs
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _   []        = []
myZipWith _ []  _         = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)
```
[Solution file](exercise.files/zipWithZipWith.hs)


# Chapter Exercises

The first set of exercises here will mostly be review but will also introduce you to some new things. The second set is more conceptually challenging but does not use any syntax or concepts we haven't already studied. If you get stuck, it may help to flip back to a relevant section and review.

## Data.Char

The first exercises are straightforward but will introduce you to some new library functions and review some of what we've learned so far. Some of the functions we will use here are not standard in `Prelude` and so have to be imported from a module called `Data.Char`. You may do so in a source file (recommended) or at the `Prelude` prompt with the same phrase: `import Data.Char` (write that at the top of your source file). This brings into scope a bunch of new standard functions we can play with that operate on `Char` and `String` types.

1. Query the types of `isUpper` and `toUpper`.

```hs
isUpper :: Char -> Bool
toUpper :: Char -> Char
```
2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a `String`? Write that funnction such that, given the input `HbEfLrLxO`, your function will return `HELLO`.
```REPL
Prelude Data.Char> isUpper 'J'
True
Prelude Data.Char> toUpper 'j'
'J'
```

```hs
import Data.Char (isUpper)

filterCapLetters :: [Char] -> [Char]
filterCapLetters = filter isUpper
```
[Solution file](capitalizeFuncs.hs)

3. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument "julie", it will return "Julie".

```hs
import Data.Char (toUpper)

capFirstLetter :: [Char] -> [Char]
capFirstLetter []     = []
capFirstLetter (x:xs) = toUpper x : xs
```
[Solution file](capitalizeFuncs.hs)


4. Now make a new version of that function that is recursive such that if you give it the input "woot" it will holler back at you "WOOT". The type signature won't change, but you will want to add a base case.

```hs
import Data.Char (toUpper)

capAllLetters :: [Char] -> [Char]
capAllLetters []      = []
capAllLetters (x:xs)  = toUpper x : capAllLetters xs
```

5. To do the final exercise in this section, we'll need another standard function for lists called `head`. Query the type of `head` and experiment with it to see what it does. Now write a function that will capitalize the first letter of a string and return only that letter as a result.

```hs
getFirstLetterCapitalized :: [Char] -> Char
getFirstLetterCapitalized []      = error "empty string"
getFirstLetterCapitalized (x:xs)  = toUpper x
```

6. Cool. Good work. Now rewrite it as a composed function. The, for fun, rewrite it pointfree.

```hs
getFirstCapitalizedComposed :: [Char] -> Char
getFirstCapitalizedComposed []  = error "empty string"
getFirstCapitalizedComposed s   = toUpper . head $ s

getFirstCapitalizedPointFree :: [Char] -> Char
getFirstCapitalizedPointFree    = toUpper . head
```


## Ciphers

We'll still be using `Data.Char` for this next exercise. You should save these exercises in a module called `Ciphers` because we'll be coming back to them in later chapters. You'll be writing a Caesar cipher for now, but we'll suggest some variations on the basic program in later chapters.

A Caesar cipher is a simple substitution cipher, in which each letter is replaced by the letter that is a fixed number of places down the alphabet from it. You will find variations on this all over the place — you can shift leftward or rightward, for any number of spaces. A rightward shift of 3 means that 'A' will become 'D' and 'B' will become 'E', for example. If you did a leftward shift of 5, then 'a' would become 'v' and so forth.

Your goal in this exercise is to write a basic Caesar cipher that shifts rightward. You can start by having the number of spaces to shift fixed, but it's more challenging to write a cipher that allows you to vary the number of shifts so that you can encode your secret messages diﬀerently each time.

There are Caesar ciphers written in Haskell all over the internet, but to maximize the likelihood that you can write yours without peeking at those, we'll provide a couple of tips.  When yours is working the way you want it to, we would encourage you to then look around and compare your solution to others out there.

The first lines of your text file should look like this:
```hs
module Cipher where

import Data.Char
```
`Data.Char` includes two functions called `ord` and `chr` that can be used to associate a `Char` with its `Int` representation in the Unicode system and vice versa:
```REPL
*Cipher> :t chr
chr :: Int -> Char
*Cipher> :t ord
ord :: Char -> Int
```
Using these functions is optional; there are other ways you can proceeed with shifting, but using `chr` and `ord` might simplify the process a bit.

You want your shift to wrap back around to the beginning of the alphabet, so that if you have a rightward shift of 3 from 'z,' you end up back at 'c' and not somewhere in the vast Unicode hinterlands. Depending on how you've set things up, this might be a bit tricky. Consider starting from a base character (e.g., 'a') and using 'mod' to ensure you're only shifting over the 26 standard characters of the English alphabet.

You should include an `unCaesar` function that will decipher your text as well. In a later chapter, we will test it.
[Solution file](exercise.files/cipher.hs)


## Writing your own standard functions
Below are the outlines of some standard functions. The goal here is to write your own versions of these to gain a deeper understanding of recursion over lists and how to make functions flexible enough to accept a variety of inputs. You could figure out how to look up the answers, but you won't do that because you know you'd only be cheating yourself out of the knowledge. Right?

Let's look at an example of what we're after here. The `and` function can take a list of `Bool` values and returns `True` if and only if no values in the list are `False`. Here's how you might write your own version of it:
```hs
-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs


-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd xs
```

1. `myOr` returns True if any `Bool` in the list is `True`.

```hs
-- implementing the standard `or` functions
-- direct recursion, not using (||)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) =
  if x then True else myOr xs

-- direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 []      = False
myOr2 (x:xs)  = x || myOr2 xs
``` 

2. `myAny` returns `True` if `a -> Bool` applied to any of the values in the list returns `True`.
```hs
-- implementation of standard function `any`
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []      = False
myAny f (x:xs)  = if f x then True else myAny f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ []     = False
myAny2 f (x:xs) = f x || myAny f xs
```

3. After you write the recursive `myElem`, write another version that uses `any`. The built-in version of `elem` in GHC 7.10 and newer has a type that used `Foldable` instead of the list type specifically. You can ignore that and write the concrete version that works only for list.

```hs
-- implementation of standard function `elem`
myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem el (x:xs)  = el == x || myElem el xs


myElem2 :: Eq a => a -> [a] -> Bool
myElem2 el = any (== el)
```

4. Implement `myReverse`.

```hs
-- implementation of standard `reverse`
myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = myReverse xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 []       = []
myReverse2 (x:xs)   = concat [myReverse2 xs, [x]]
```