# Solutions to problems of chapter 10

## Understanding Folds

1. `foldr (*) 1 [1..5]`

will return the same result as which of the following:
<br>a.`flip (*) 1 [1..5]`
<br>b. `foldl (flip (*)) 1 [1..5]`
<br>c. `foldl (*) 1 [1..5]`

**Answer:** Both `b` and `c` return the same. The main reason is that multiplication is an _associative_ operation.

2. Write out the evaluation steps for

`foldl (flip (*)) 1 [1..3]`

```hs
-- f ~ (flip (*))
(((1 `f` 1) `f` 2) `f` 3)
(((flip (*) 1 1) `f` 2) `f`) 3)
(((1) `f` 2) `f` 3)
((flip (*) 1 2) `f` 3)
((2 * 1) `f` 3)
2 `f` 3
flip (*) 2 3
3 * 2
6
```

3. One difference between `foldr` and `foldl` is:
<br>a. `foldr`, but not `foldl`, traverses the spine of a list from right  to left.
<br>b. `foldr`, but not `foldl`, always forces the rest of the fold.
<br>c. `foldr`, but not `foldl`, associates to the right.
<br>d. `foldr`, but not `foldl`, is recursive.

**Answer:** Only `c` is correct.

4. Folds are catamorphisms, which means they are generally used to:
<br>a. reduce structure
<br>b. expand structure
<br>c. render you catatonic
<br>d. generate infinite data structures

**Answer:** Only `a`.

5. The following are simple folds very similar to what you've already seen, but each has at least one error. Please fix them and test in your REPL:
<br>a. `foldr (++) ["woot", "WOOT", "woot"]`
<br>**Answer:** It lacks the initial value. `foldr` function signature is `foldr :: (a -> b -> b) -> a -> [a] -> b`. To correct it we write: `foldr (++) "" ["woot", "WOOT", "woot"]`
<br>b. `foldr max [] "fear is the little death"`
<br>**Answer:** `max` has the signature `max :: Ord a => a -> a -> a`. This means that the type of the elements of the list and the type of the initial value must be the same and it must have an instance of the `Ord` typeclass. Since a Haskell `String` is just a `[Char]` the initial value must be of type `Char`. To correct it we write: `foldr max 'a' "fear is the little death"`.
<br>c. `foldr and True [False, True]`
<br>**Answer:** `and` has the signature `and :: [Bool] -> Bool`. This does not fit the function signature `foldr` expects. To correct it we write: `foldr (&&) True [False, True]`
<br>d. This one is more subtle than the previous. Can it ever return a different answer?
<br>`foldr (||) True [False, True]`
<br>**Answer:** The above expression will reduce to `True` unless every elemnt in the list and also the initial value are _all_ `False`.
<br>e. `foldl ((++) . show) "" [1..5]`
<br>**Answer:** `foldl (flip $ (++) . show) "" [1..5]`
<br>f. `foldr const 'a' [1..5]`
<br>**Answer:** `foldr` has the type `foldr :: (a -> b -> b) -> b -> [a]`. `const` has type `const :: a -> b -> a`. To correct it we write: `foldr (flip const) 'a' [1..5]`.
<br>g. `foldr const 0 "tacos"`
<br>**Answer:** Same as the previous problem. To solve the problem we write: `foldr (flip const) 0 "tacos"`
<br>h. `foldl (flip const) 0 "burritos"`
<br>**Answer:** Same as previous problem. `foldl const 0 "burritos"`
<br>i. `foldl (flip const) 'z' [1..5]`
<br>**Answer:** Same as previous problem. `foldl const 'z' [1..5]`


## Database Processing

Write the following functions for processing this data.

```hs
import Data.Time

data DatabaseItem = DbString    String
                  | DbNumber    Integer
                  | DbDate      UTCTime 
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate $ UTCTime  (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123)
    , DbNumber 9001
    , DbString "Hello, World!"
    , DbDate $ UTCTime  (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123)
    ]
```

1. Write a function that filters for `DbDate` values and returns a list of the `UTCTime` values inside them.
```hs
filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldr f []
  where 
        f (DbDate a) b = a : b
        f  _         b = b
```
[Solution file](exercise.files/theDatabase.hs)

2. Write a function that filters for `DbNumber` values and returns a list of the `Integer` values inside them.

```hs
filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr f []
  where
        f (DbNumber a) b = a : b
        f _            b = b
```
[Solution file](exercise.files/theDatabase.hs)


3. Write a function that gets the most recent date.

```hs
mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = foldr f z
  where
        f (DbDate a) b = if a > b then a else b
        f _          b = b
        z              = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
```
[Solution file](exercise.files/theDatabase.hs)


4. Write a function that sums all of the `DbNumber` values.

```hs
sumDb :: [DatabaseItem]
      -> Integer
sumDb = foldr f 0
  where
        f (DbNumber a) b = a + b
        f _            b = b
```
[Solution file](exercise.files/theDatabase.hs) 


5. Write a function that gets the average of the DbNumber values.

```hs
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem]
      -> Double
avgDb xs = calc (go xs)
  where
        calc (s, 0) = fromIntegral s
        calc (s, l) = fromIntegral s / fromIntegral l
        go xs = foldr f (0, 0) xs
          where
                f (DbNumber a) (b, c) = (b+a, c+1)
                f _            (b, c) = (b, c)
```
[Solution file](exercise.files/theDatabase.hs)


## Scans

1. Modify your `fibs` function to only return the first 20 Fibonacci numbers.

```hs
-- creates an infinite list of fibonacci numbers
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- returns the first 20 fibonacci numbers
fibsFirst20 :: [Integer]
fibsFirst20 = take 20 fibs
```
[Solution file](exercise.files/fibs.hs)

2. Modify `fibs` to return the Fibonacci numbers that are less than 100.

```hs
-- returns all fibonacci numbers less than 100
fibsLessThan100 :: [Integer]
fibsLessThan100 = takeWhile (<100) fibs
```
[Solution file](exercise.files/fibs.hs)

3. Try to write the `factorial` function from Recursion as a `scan`. You'll want `scanl` again, and your start value will be `1`. Warning: this will also generate an infinite list, so you may want to pass it through a `take` function or similar.

```hs
-- creates an infinite list of factorial numbers
fac :: [Integer]
fac = scanl (*) 1 [2..]

-- returns the first n factorial numbers
facN :: Int -> [Integer]
facN n = take n fac

-- pointfree style of the above function
facN2 :: Int -> [Integer]
facN2 = flip take fac
```
[Solution file](exercise.files/fibs.hs)

# Chapter exercises

## Warm-up and review
For the following set of exercises, you are not expected to use folds. These are intended to review material from previous chapters. Feel free to use any syntax or structure from previous chapters that seems appropriate.

1. Given the following sets of consonants and vowels:

```hs
stops = "pbtdkg"
vowels = "aeiou"
```
a. Write a function that takes inputs from `stops` and `vowels` and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

```hs
stops = "pbtdkg"
vowels = "aeiou"

create3Tuples :: String -> String -> [(Char, Char, Char)]
create3Tuples s v = [(a, b, c) | a <- stops, b <- vowels, c <- stops]
```
[Solution file](exercise.files/stopVowelStop.hs)
<br>b. Modify that function so that it only returns the combinations that begin with a `p`.

```hs
create3TuplesStartingWithP :: String -> String -> [(Char, Char, Char)]
create3TuplesStartingWithP s v = [('p', b, c) | b <- v, c <- s]
```
[Solution file](exercise.files/stopVowelStop.hs)
<br>c. Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
```hs

verbs = ["Act","Answer","Approve","Arrange","Break","Build","Buy","Coach"
        ,"Color","Cough","Create","Complete","Cry","Dance","Describe","Draw"
        ,"Drink","Eat","Edit","Enter","Exit","Imitate","Invent","Jump","Laugh"
        ,"Lie","Listen","Paint","Plan","Play","Read","Replace","Run","Scream"
        ,"See","Shop","Shout","Sing","Skip","Sleep","Sneeze","Solve","Study"
        ,"Teach","Touch","Turn","Walk","Win","Write","Whistle","Yank","Zip"]
nouns = ["Actor","Gold","Painting","Advertisement","Grass","Parrot","Afternoon"
        ,"Greece","Pencil","Airport","Guitar","Piano","Ambulance","Hair","Pillow"
        ,"Animal","Hamburger","Pizza","Answer","Helicopter","Planet","Apple"
        ,"Helmet","Plastic","Army","Holiday","Portugal","Australia","Honey","Potato"
        ,"Balloon","Horse","Queen","Banana","Hospital","Quill","Battery","House"
        ,"Rain","Beach","Hydrogen","Rainbow","Beard","Ice","Raincoat","Bed","Insect"
        ,"Refrigerator","Belgium","Insurance","Restaurant","Boy","Iron","River"
        ,"Branch","Island","Rocket","Breakfast","Jackal","Room","Brother","Jelly"
        ,"Rose","Camera","Jewellery","Russia","Candle","Jordan","Sandwich","Car"
        ,"Juice","School","Caravan","Kangaroo","Scooter","Carpet","King","Shampoo"
        ,"Cartoon","Kitchen","Shoe","China","Kite","Soccer","Church","Knife","Spoon"
        ,"Crayon","Lamp","Stone","Crowd","Lawyer","Sugar","Daughter","Leather"
        ,"Sweden","Death","Library","Teacher","Denmark","Lighter","Telephone"
        ,"Diamond","Lion","Television","Dinner","Lizard","Tent","Disease","Lock"
        ,"Thailand","Doctor","London","Tomato","Dog","Lunch","Toothbrush","Dream"
        ,"Machine","Traffic","Dress","Magazine","Train","Easter","Magician","Truck"
        ,"Egg","Manchester","Uganda","Eggplant","Market","Umbrella","Egypt","Match"
        ,"Van","Elephant","Microphone","Vase","Energy","Monkey","Vegetable","Engine"
        ,"Morning","Vulture","England","Motorcycle","Wall","Evening","Nail","Whale"
        ,"Eye","Napkin","Window","Family","Needle","Wire","Finland","Nest","Xylophone"
        ,"Fish","Nigeria","Yacht","Flag","Night","Yak","Flower","Notebook","Zebra"
        ,"Football","Ocean","Zoo","Forest","Oil","Garden","Fountain","Orange","Gas"
        ,"France","Oxygen","Girl","Furniture","Oyster","Glass","Garage","Ghost"]
        
createNounVerbNounTuples :: [String] -> [String] -> [(String, String, String)]
createNounVerbNounTuples n v = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]
```
[Solution file](exercise.files/stopVowelStop.hs)

2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.

```hs
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
```

This function returns the average (mean) length of words truncated towards 0. `length (words x)` returns the number of words in `x`, `map length (words x)` returns a list containing the length of each individual word. Taking the `sum` of it gives the number of characters in all the words combined. Their `div` is the number of characters in all the words combined integer divided by the number of individual words.

3. We'd really like the answer to be more precise. Can you rewrite that using fractional division?

```hs
seekritFun2 x =
  (/) (fromIntegral $ sum $ map length $ words x)
      (fromIntegral $ length $ words x)
```

## Rewriting functions using folds

In the previous chapter, you wrote these functions using direct recursion over lists. The goal now is to rewrite them using folds. Where possible, to gain a deeper understanding of folding, try rewriting the fold version so taht it is point-free.

Point-free versions of these functions written with a fold should look like:
```hs
myFunc = foldr f z
```
So for example with the `and` function:

```hs
-- Again, this type will be less
-- reusable than the one in GHC 7.10
-- and newer. Don't worry.

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd []    = True
myAnd (x:xs)=
  if x == False
  then False
  else myAnd xs

-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 []         = True
myAnd2 (x:xs)     = x && myAnd xs


-- fold, not point-free
-- in the folding function
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr
            (\a b ->
             if a == False
             then False
             else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr && True
```

The goal here is to converge on the final version where possible. You don't need to write all variations for each example, but the more variations you write, the deeper your understanding of these function will become.

1. `myOr` returns `True` if any `Bool` in the list is `True`.
```hs
-- recursive
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- foldr
myOr2 :: [Bool] -> Bool
myOr2 xs = foldr (||) False xs

-- foldr point-free
myOr3 :: [Bool] -> Bool
myOr3 = foldr (||) False
```
[Solution file](exercise.files/recursiveToFold.hs)

2. `myAny` returns `True` if `a -> Bool` applied to any of the values in the list returns True.

```hs
-- standard any implementations
-- recursive
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []        = False
myAny p (x:xs)    = p x || myAny p xs

-- foldr
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 p xs = foldr (\a b -> p a || b) False xs

-- foldr
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 p xs = foldr (||) False $ map p xs


-- foldr using function composition
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 p xs = foldr ((||) . p) False xs

-- foldr point-free
myAny5 :: (a -> Bool) -> [a] -> Bool
myAny5 p   = foldr ((||) . p) False
```
[Solution file](exercise.files/recursiveToFold.hs)


3. Write two versions of `myElem`. One version should use folding and the other should use `any`.

```hs
-- standard elem implementations
-- recursive
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys)
  | x == y      = True
  | otherwise   = myElem x ys
  

-- foldr
myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 x xs = foldr (\a b -> x == a || b) False xs

-- foldr, point-free
myElem3 :: (Eq a) => a -> [a] -> Bool
myElem3 x = foldr (\a b -> x == a || b) False

-- foldr, map
myElem4 :: (Eq a) => a -> [a] -> Bool 
myElem4 x xs = foldr (||) False $ map (==x) xs 

-- foldr, function composition
myElem5 :: (Eq a) => a -> [a] -> Bool
myElem5 x xs = foldr ((||) . (==x)) False xs

-- foldr, function composition, point-free
myElem6 :: (Eq a) => a -> [a] -> Bool 
myElem6 x = foldr ((||) . (==x)) False

-- using any
myElem7 :: (Eq a) => a -> [a] -> Bool 
myElem7 x = any (== x)
```
[Solution file](exercise.files/recursiveToFold.hs)


4. Implement myReverse, don't worry about trying to make it lazy.

```hs
-- standard reverese implementations
-- recursive
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- foldl
myReverse2 :: [a] -> [a]
myReverse2 xs = foldl (\acc x -> x : acc) [] xs

-- foldl without lambda
myReverse3 :: [a] -> [a]
myReverse3 xs = foldl (flip (:)) [] xs

-- foldl, point-free
myReverse4 :: [a] -> [a]
myReverse4 = foldl (flip (:)) []
```
[Solution file](exercise.files/recursiveToFold.hs)


5. Write `myMap` in terms of `foldr`. It should have the same behavior as the built-in `map`.

```hs
-- standar map implementations
-- recursive
myMap :: (a -> b) -> [a] -> [b]
myMap _ []      = []
myMap f (x:xs)  = f x : myMap f xs

-- foldr
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 _ []     = []
myMap2 f xs = foldr (\a acc -> (f a) : acc) [] xs

-- foldr, eta-reduced
myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr (\a acc -> (f a) : acc) []

-- foldr, lambda-free
myMap4 :: (a -> b) -> [a] -> [b]
myMap4 f xs = foldr ((:) . f) [] xs

-- foldr, lambda-free, eta-reduced
myMap5 :: (a -> b) -> [a] -> [b]
myMap5 f = foldr ((:) . f) []
```
[Solution file](exercise.files/recursiveToFold.hs)


6. Write `myFilter` in terms of `foldr`. It should have the same behavior as the built-in `filter`.

```hs
-- standard filter implementations
-- recursive
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs) = if p x then x : myFilter p xs else myFilter p xs

-- foldr
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p xs = foldr (\a acc -> if p a then a : acc else acc) [] xs

-- foldr, eta-reduced
myFilter3 :: (a -> Bool) -> [a] -> [a]
myFilter3 p = foldr (\a acc -> if p a then a : acc else acc) []
```
[Solution file](exercise.files/recursiveToFold.hs)

7. `squish` flattens a list of lists into a list.

```hs
-- standard concat impplementations
-- recursive
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- foldr
squish2 :: [[a]] -> [a]
squish2 xs = foldr (++) [] xs

-- foldr, point-free
squish3 :: [[a]] -> [a]
squish3 = foldr (++) []
```

8. `squishMap` maps a function over a list and concateneates the results.

```hs
-- standard concatMap impplementations
-- recursive
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- foldr
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f xs = foldr (\a acc -> f a ++ acc) [] xs

-- foldr, eta-reduced
squishMap3 :: (a -> [b]) -> [a] -> [b]
squishMap3 f = foldr (\a acc -> f a ++ acc) []

-- foldr, lambda-free
squishMap4 :: (a -> [b]) -> [a] -> [b]
squishMap4 f xs = foldr ((++) . f) [] xs

-- foldr, lambda-free, eta-reduced
squishMap5 :: (a -> [b]) -> [a] -> [b]
squishMap5 f = foldr ((++) . f) []
```
[Solution file](exercise.files/recursiveToFold.hs)



9. `squishAgain` flattens a list of lists into a list. This time re-use the `squishMap` function.
```hs
-- standard concat implementations relying on concatMap
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\a -> a) xs

-- lambda-free
squishAgain2 :: [[a]] -> [a]
squishAgain2 xs = squishMap id xs

-- lambda-free, eta-reduced
squishAgain3 :: [[a]] -> [a]
squishAgain3 = squishMap id
```
[Solution file](exercise.files/recursiveToFold.hs)


10. `myMaximumBy` takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned `GT` for.

```hs
-- standard maximumBy implementations
-- recursive
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ []    = error "list of length zero"
myMaximumBy f xs     = go f (head xs) xs where
  go f r []         = r
  go f r (x:xs) 
    | f x r == GT   = go f x xs
    | otherwise     = go f r xs 
    
-- foldr
myMaximumBy2 :: (a -> a -> Ordering)
             -> [a] -> a
myMaximumBy2 _ []     = error "list of length zero"
myMaximumBy2 f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
```
[Solution file](exercise.files/recursiveToFold.hs)


11. `myMinimumBy` takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned `LT` for.


```hs
--standard minimumBy implementations
-- recursive
myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ []  = error "list of length zero"
myMinimumBy f xs   = go f (head xs) xs where
  go f r []       = r
  go f r (x:xs)
    | f x r == LT = go f x xs
    | otherwise   = go f r xs

-- foldr
myMinimumBy2 :: (a -> a -> Ordering)
             -> [a] -> a
myMinimumBy2 _ []     = error "list of length zero"
myMinimumBy2 f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs
```
[Solution file](exercise.files/recursiveToFold.hs)