# Solutions to problems of chapter 12

# Chapter exercises

## Determin the kinds


1. Given

```hs
id :: a -> a
```
What is the kind of `a`?
```hs
a :: *
```


2.
```hs
r :: a -> f a
```
 What are the kinds of `a` and `f`?
 ```
 a :: *
 f :: * -> *
```

## String processing

Because this is the kind of thing linguists _ahem_ enjoy doing in their spare time.

1. Write a recursive function named `replaceThe` which takes a text/string, breaks it into words and replaces each instance of "the" with "a". It's intended only to replace exactly the word "the". `notThe` is a suggested helper function for accomplishing this.

```hs
-- example input and output for `replaceThe`
-- >>> replaceThe "the cow loves us"
-- "a cow loves us"

-- takes a string and replaces all occrences of 'the'
-- with 'a' ignoring case
-- example: replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words


-- example input and output for `notThe`
-- >>> nothThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

-- takes a string as a word and returns Nothing if it's the
-- otherwise returns Just the inputstring
notThe :: String -> Maybe String
notThe word
  | word == "the" = Nothing
  | otherwise = Just word
```
[Solution file](exercise.files/string.hs)


2. Write a recursive function that takes a text/string, breaks it into works, and counts the number of instances of "the" followed by a vowel-initial word.
```hs
-- example input and output for countTheBeforeVowel
-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> CountTheBeforeVowel "the evil cow"
-- 1

-- takes a string and returns the number of "the"s
-- where the next word starts with a vowel
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . foldr go ("", 0) . words where
  go = \word (nextWord, count) -> if not (null nextWord) && toLower (head nextWord) `elem` "aeoiuy" && map toLower word == "the" then (word, count+1) else (word, count)
  

-- same as countTheBeforeVowel
-- explicit recursion
countTheBeforeVowelRec :: String -> Integer 
countTheBeforeVowelRec xs = snd  (go (words xs)) where
  go :: [String] -> (String, Integer)
  go [] = ("", 0)
  go (x:xs) 
   | not (null nextWord) && toLower (head nextWord) `elem` "aeoiuy" && map toLower x == "the" = (x, count+1)
   | otherwise = (x, count)
   where
     (nextWord, count) = go xs
```
[Solution file](exercise.files/string.hs)

3. Return the number of letters that are vowels in a word. Hint: it's helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

<br>a. Test for vowelhood
<br>b. Return the vowels of a string
<br>c. Count the number of elements returned

```hs
-- example input and output for countVowels
-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
-- takes a string and returns the number of vowels in it
countVowels :: String -> Integer
countVowels = foldr (\x acc -> if x `elem` "aeiouy" then acc+1 else acc) 0




-- same as countVowels
-- explicit recursion
countVowelsRec :: String -> Integer 
countVowelsRec []      = 0
countVowelsRec (x:xs)
  | toLower x `elem` "aeoiuy" = 1 + countVowelsRec xs
  | otherwise                 = countVowelsRec xs
```
[Solution file](exercise.files/string.hs)


## Validate the word
Use the `Maybe` type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns `Nothing`. In many human languages, vowels rarely exceed the number of consonants so when they do, it _may_ indicate the input isn't a word (that is, a valud input to your dataset).

```hs
newtype Word' =
  Word' String 
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word = if    countConsonants > countVowels
              then  Nothing 
              else  Just (Word' word) where
    (countConsonants, countVowels) =
      foldr f (0, 0) word where
      f x (v, c) = if x `elem` vowels
                   then (v+1, c)
                   else (v, c+1)
```
[Solution file](exercise.files/validateWord.hs)


## It's only natural

You'll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert `Naturals` to `Integers` and `Integers` to `Naturals`. The conversion from `Naturals` to `Integers` won't return `Maybe` because `Integer` is a strict superset of `Natural`. Any `Natural` can be represented by an `Integer`, but the same is _not_ true of any `Integer`. Negative numbers are not valid natural numbers.

```hs
-- As natural as any
-- competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)
  
-- example input and output of natToInteger
-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Suc (Succ Zero))
-- 2
-- takes a Nat and returns its Integer value
natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n


-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >> IntegerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0       = Nothing
  | otherwise   = Just (go n) where
    go n
      | n == 0        = Zero
      | otherwise     = Succ (go (n-1))
```
[Solution file](exercise.files/naturals.hs)


## Small library for Maybe

Write the following functions. This may take some time.

1. Simple boolean checks for `Maybe` values.

```hs
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False


-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust
```
[Solution file](exercise.files/maybe.hs)


2. The following is the `Maybe` catamorphism. You can turn a `Maybe` value into anything else with this.

```hs
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing       = b
mayybee b f (Just a)      = f a
```
[Solution file](exercise.files/maybe.hs)

3. In case you just want to provide a fallback value.

```hs
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe b = mayybee b id
```
[Solution file](exercise.files/maybe.hs)


4. Converting between `List` and `Maybe`.

```hs
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x:xs)  = Just x


-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just a)  = [a]
```
[Solution file](exercise.files/maybe.hs)


5. For when we want to drop the `Nothing values from out list.

```hs
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >> xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList
```
[Solution file](exercise.files/maybe.hs)

6. You'll see this called "sequence" later.


```hs
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []      = Just []
flipMaybe (Nothing:xs)  = Nothing
flipMaybe ((Just a):xs) = fmap (a :) (flipMaybe xs)
```
[Solution file](exercise.files/maybe.hs)


## Small library for Either

Write each of the following functions. If more than one possible unique function exists for the type, use common sense eto determine what it should do.

1. Try to eventually arrive at a solution that uses `foldr`, even if earlier versions don't use `foldr`.

```hs
-- takes a list of Either and
-- returns a list of all the inhabitants of Lefts
-- implementation of the standard left function in Data.Either
lefts' :: [Either a b] -> [a]
lefts' []             = []
lefts' ((Left x):xs)  = x : lefts' xs
lefts' (_:xs)         = lefts' xs


-- same as above but using foldr
leftsFoldr' :: [Either a b] -> [a]
leftsFoldr' = foldr f [] where
  f (Left x) acc  = x : acc
  f _ acc         = acc
```
[Solution file](exercise.files/either.hs)

2. Same as the last exercise but for `Right`. Use `foldr` eventually.

```hs
-- takes a list of Either and
-- returns a list of all the inhabitants of Rights
-- implementation of the standard right function in Data.Either
rights' :: [Either a b] -> [b]
rights' []              = []
rights' ((Right x):xs)  = x : rights' xs
rights' (_:xs)          = rights' xs


-- same as above but using foldr
rightsFoldr' :: [Either a b] -> [b]
rightsFoldr' = foldr f [] where
  f (Right x) acc = x: acc
  f _ acc         = acc
  ```
  [Solution file](exercise.files/either.hs)
  

3. Implement the standard `partitionEithers` function from `Data.Either`.

```hs
-- implementation of the standard function
-- partitionEithers in Data.Either
partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' []                = ([], [])
partitionEithers' (x:xs) = either (\a -> (a:as, bs)) (\b -> (as, b:bs)) x
  where (as, bs) = partitionEithers' xs
  
-- same as above but using foldr
partitionEithersFoldr'  :: [Either a b]
                        -> ([a], [b])
partitionEithersFoldr' = foldr (\x (as, bs) -> either (\a -> (a:as, bs)) (\b -> (as, b:bs)) x) ([],[])
```
[Solution file](exercise.files/either.hs)


4. Take a function `(b -> c)` and `Either a b` and return `Maybe c`.

```hs
-- take a function (b -> c) and Either a b
-- return Maybe c
eitherMaybe'  :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe' f (Right b)  = Just (f b)
eitherMaybe' _ _          = Nothing
```
[Solution file](exercise.files/either.hs)


5. This is a general catamorphism for `Either` values.

```hs
-- implementation of the standard function either
-- take 2 function (a -> c), (b -> c) and Either a b
-- return c
either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' l _ (Left x)  = l x
either' _ r (Right x) = r x
```
[Solution file](exercise.files/either.hs)


6. Same as before, but use the `either'` function you just wrote.

```hs
-- same as the eitherMaybe' above
-- but using either'
eitherMaybe'' ::  (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
```

