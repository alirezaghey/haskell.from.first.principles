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