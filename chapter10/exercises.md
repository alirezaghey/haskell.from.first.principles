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