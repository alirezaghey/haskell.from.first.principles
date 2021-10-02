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

