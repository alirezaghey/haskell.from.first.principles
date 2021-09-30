# Solutions to problems of chapter 11

## Dog Types

Given the following datatype:
```hs
data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDebordeaux doge
```
Answer the following questions:
1. Is `Doggies` a type constructor or a data constructor? **Type constructor**
2. What is the kind of `Doggies`? `* -> *`
3. What is the kind of `Doggies String`? `*`
4. What is the type of `Husky 10`? `Num a => Doggies a`
5. What is the type of `Husky (10 :: Integer)`? `Doggies Integer`
6. What is the type of `Mastiff "Scooby Doo"`? `Doggies String`
7. Is `DogueDebordeaux` a type constructor or a data constructor? Both.
8. What is the type of `DogueDeBordeaux`? `DogueDeBordeaux :: doge -> DogueDeBordeaux doge`
9. What is the type of `DogueDeBordeaux "doggie!"`? `DogueDeBordeaux "doggie!" :: DogueDeBordeaux String`


## Vehicles

For these exercises, we'll use the datatypes defined in the above section. It would be good if you'd typed them all into a source file already, but if you hadn't, please do so now. You can then define some sample data on your own, or use these to get you started:
```hs
data Price = Price Integer
              deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
                      deriving (Eq, Show)

data Airline = PapuAir | CXatapultsR'Us | TakeYourChancesUnitd
                deriving (Eq, Show)


data Vehicle =  Car Manufacturer Price
              | Plane Airline
                  deriving (Eq, Show)


myCar     = Car Mini (Price 14000)
urCar     = Car Mazda (Price 20000)
clownCar  = Car Tata (Price 7000)
doge      = Plane PapuAir
```
1. What is the type of `myCar`? `myCar :: Vehicle`

2. Given the above, define the following functions:

```hs
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _  = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = foldr (\a acc -> (isCar a):acc) []

areCars2 :: [Vehicle] -> [Bool]
areCars2 = foldr ((:) . isCar) []

areCars3 :: [Vehicle] -> [Bool]
areCars3 = map isCar
```
[Solution file](exercise.files/vehicles.hs)

3. Now we're going to write a function to tell us the manufacturer of a piece of data:

```hs
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
```
[Solution file](exercise.files/vehicles.hs)

4. Given that we're returning the `Manufacturer`, what will hapen if you use this on `Plane` data? Run time throws an exception since our patterns are non-exhaustive.

5. All right. Let's say you've decided to add the size of the plane as an argument to the `Plane` constructro. Add that to your datatypes in the appropriate places and change your data functions appropriately.
[Solution file](exercise.files/vehicles.hs)


## Cardinality

While we haven't explicitly described the rules for calculating the cardinality of datatypes yet, you might already have an idea of how to do it for simple datatypes with nullary constructors. Try not to overthink these exercises -- follow your intuition based on what you know.

1. `data PugType = PugData`<br>**Answer:** Cardinality of 1.
2. For this one, recall that `Bool` is also defined with the `|`:

```hs
data Airline =
        PapuAir
     |  CatapultsR'Us
     |  TakeYourChancesUnited
```
<br>**Answer:** Cardinality is 3.

3. Given what we know about `Int8`, what's the cardinality of `Int16`?<br>**Answer:** 2^16 = 65536 => 2^15 negative number and 2^15-1 positive numbers plus zero.
4. Use the REPL and `maxBound` and `minBound` to examine `Int` and `Integer`. What can you say about the cardinality of those types?<br>**Answer:** `Int` has cardinality of 2^64 = 18446744073709551616 => 2^63 negative numbers and 2^63-1 positive number plus zero.
5. Extra credit: What's the connection between the 8 in `Int8` and that type's cardinality of 256?<br>**Answer:** 2^8 = 256


## For Example

1. You can query the type of a value in GHCi with the `:type` command, also abbreviated `:t`.

```REPL
Prelude> :t False
False :: Bool
```

What is the type of data constructor `MakeExample`? What happens when you request the type of `Example`?
<br>**Answer:** The type of `Example` is `Example :: MakeExample`. When typing `:t Example` GHCi throws and error saying: `Data constructor not in scope: Example`. The reason is that `Example` is a type constructor.

2. What if you try `:info` on `Example` in GHCi? Can you determine what typeclass instances are defined for the `Example` type using `:info` in GHCi?
<br>**Answer:**Yes. `Show` instance is defined for `Example`.

3. Try making a new datatype like `Example` but with a single type argument added to `MakeExample`, such as `Int`. What has changed when you query `MakeExample` with `:type` in GHCi?
<br>**Answer:**
```hs
data Example2 = MakeExample2 Int deriving (Show, Eq, Ord)
```
Requsting `:type MakeExample2` in GHCi shows: `MakeExample2 :: Int -> Example2`. It means that the data constructor `MakeExample2` takes one `Int` argument and create an instance of type `Example2`.

## Logic Goats

1. Reusing the `TooMany` typeclass, write an instance of the typeclass for the type `(Int, String)`. This will require adding a language pragma named `FlexibleInstances` _if_ you do not used a newtype -- GHC will tell you what to do.

```hs
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool 

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n
```
[Solution file](exercise.files/tooManyTypeclass.hs)


2. Make another `TooMany` instance for `(Int, Int)`. Sum the values together under the assumption this is a count of goats from two fields.

```hs
instance TooMany (Int, Int) where
  tooMany (n , m) = tooMany (n + m)
```
[Solution file](exercise.files/tooManyTypeclass.hs)

3. Make another `TooMany` instance, this time for `(Num a, TooMany a) => (a, a)`. This can mean whatever you want, such as summing the two numbers together.

```hs
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany n && tooMany m
```
[Solution file](exercise.files/tooManyTypeclass.hs)

## Pity the Bool

1. Given a datatype

```hs
data BigSmall =
      Big Bool
    | Small Bool
    deriving (Eq, Show)
```
What is the cardinality of this datatype? Hint: We already know `Bool`'s cardinality. Show your work as demonstrated earlier.
<br>**Answer:** 
```
Big Bool | Small Bool =
(Big True | Big False) + (Small True | Small False) =
(1 + 1) + (1 + 1) =
4
```

2. Given a datatype

```hs
-- bring Int8 in scope
import Data.Int

data NumberOrBool =
      Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
myNumba = Numba (-128)
```

What is the cardinality of `NumberOrBool`? What happens if you try to create a `Numba` with a numeric literal larger than `127`? And with numeric literals smaller than `(-128)`?

The cardinality of `NumberOrBool`:
```
Numba Int8 | BoolyBool Bool =
256 + 2 = 258
```
What happens if you try to create a `Numba` with numeric literals out of the `Int8` min/max bounds? It overflows/underflows


## How Does Your Garden Grow?

1. Given the type

```hs
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show
```
What is the sum of products normal form of `Garden`?
<br>**Answer:** 
```hs
data GardenNormal =
    Gardenia' Gardener
  | Daisy'    Gardener
  | Rose'     Gardener
  | Lilac'    Gardener
  deriving Show
```

## Programmers

Write a funciton that generates all possible values of `Programmer`. Use the provided lists of inhabitants of `OperatingSystem` and `ProgLang`.

```hs
module Programmers where

data OperatingSystem =
        GnuPlusLinux
      | OpenBSDPlusNevermindJustBSDStill
      | Mac
      | Windows
      deriving (Eq, Show)

data ProgLang =
        Haskell
     |  Agda
     |  Idris
     |  PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]
  
allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

main :: IO ()
main = do
  print allProgrammers
```
Please note that we could also create all the possible `OperatingSystem`s and `ProgLang`s without hardcoding them into lists. In order to do so, we have to derive instances of the `Enum` typeclass for both types and then we could enumerate over all the data constructors. Something as follows:
```hs
data OperatingSystem =
        GnuPlusLinux
      | OpenBSDPlusNevermindJustBSDStill
      | Mac
      | Windows
      deriving (Eq, Show, Enum)

data ProgLang =
        Haskell
     |  Agda
     |  Idris
     |  PureScript
     deriving (Eq, Show, Enum)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)
  
allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | lang <- enumFrom Haskell, os <- enumFrom GnuPlusLinux]
print allProgrammers
```


## The Quad

Determine how many unique inhabitants each type has.

Suggestion: do the arithmetic unless you want to verify. Writing them out gets dedious quickly. Considering the `Quad` type as described below answer the following questions.
```hs
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)
```
1. How many different forms can this take:

```hs
eQuad :: Either Quad Quad
eQuad = ???
```
**Answer:** 2 * 4 = 8

2. `prodQuad :: (Quad, Quaad)`
<br>**Answer:** 4 * 4 = 16

3. `funcQuad :: Quad -> Quad`
<br>**Answer:** 4^4 = 2^8 = 256

4. `prodTBool :: (Bool, Bool, Bool)`
<br>**Answer:** 2 * 2 * 2 = 8

5. `gTwo :: Bool -> Bool -> Bool`
<br>**Answer:** (2^2)^2 = 2^4 = 16

6. `fTwo :: Bool -> Quad -> Quad`
<br>**Answer:** (2^4)^4 = 2^16 = 65536


## Write map for BinaryTree

Given the following definition of `BinaryTree`
```hs
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
```
write a map function for the data structure. You don't really need to know anything about binary trees to write these functions. The struture inherent in the definition of the type is all you need. All you need to do is write the recursive functions.

No special algorithms are needed and we don't expect you to keep the tree balanced or ordered. Also, remember that we've never once mutated anything. We've only built new values from input data. Given that, when you go to implement `mapTree`, you're not changing an existing tree -- you're building a new one based on an existing one (as when you are mapping functions over lists).

Note, you do _note_ need to use `insert'` for this. Retain the original structure of the tree.

```hs
mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) f a (mapTree f right)
```
[Solution file with tests](exercise.files/binaryTree.hs.hs)


## Convert binary trees to list

Write function to convert `BinaryTree` valus to lists. Make certain your implementation passes the tests.
```hs
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show
  



preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right


inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad new bears."
    

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."
    

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1,3,2]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."
```
[Solution file](exercise.files/binaryTree.hs)


## Write foldr for BinaryTree

Given the definition of `BinaryTree` we have prodived, write a catamorphism for the binary trees.
```hs
-- inorder fold
foldTree :: (a -> b -> b)
         ->  b
         ->  BinaryTree a
         ->  b
foldTree _ b Leaf   = b
foldTree f b (Node left a right) = foldTree f z right where
                                    z = f a g where
                                      g = goldTree f b left
```

# Chapter Exercises

## Multiple choice

1. Given the following datatype:

```hs
data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thrusday
  | Friday
```

we can say:
- [x] a. `Weekday` is a type with five data constructors
- [ ] b. `Weekday` is a tree with five branches
- [ ] c. `Weekday` is a product type
- [ ] d. `Weekday` takes five arguments  

2. and with the same datatype definition in mind, what is the type of the following function, `f`?

```hs
f Friday = "Miller Time"
```
- [ ] a. `f :: [Char]`
- [ ] b. `f :: String -> String`
- [x] c. `f :: Weekday -> String`
- [ ] d. `f :: Day -> Beer`


3. Types defined with the `data` keyword

- [ ] a. must have at least one argument
- [x] b. must begin with a capital letter
- [ ] c. must be polymorphic
- [ ] d. cannot be imported from modules

4. The function `g xs = xs !! (length xs - 1)`

- [ ] a. is recursive and may not terminate
- [ ] b. delivers the head of `xs`
- [x] c. delivers the final element of `xs`
- [ ] d. has the same type as `xs`

## Ciphers

In the Lists chapter, you wrote a Caesar cipher. Now, we want to expand on that idea by writing a Vigenere cipher. A Vigenere cipher is another substitution cipher, based on Caesar cipher, but it uses a series of Caesar ciphers for polyalphabetic substitution. The substitution for each letter in the plaintext is determined by a fixed keyword.

So, for example, if you want to encode the message "meet at dawn", the first step is to pick a keyword that will determine which Caesar cipher to use. We'll use the keyword "Ally" here. You repeat the kyword for as many characters as there are in your original message:
```
MEET AT DAWN
ALLY AL LYAL
```
Now the number of rightward shifts to make to encode each character is set by the character of the keyword that lines up with it. The 'A' means a shift of 0, so the initial 'M' will remain 'M'. But the 'L' for our second character sets a rightward shift of 11, so 'E' becoms 'P'. And so on, so 'meet at dawn' encoded with the keyword 'Ally' becomes 'MPPR AE OYWY'.

Like the Caesar cipher, you can find all kinds of resources to help you understand the cipher and also many examples written in Haskell. Consider using a combination of `chr`, `ord`, and `mod` again, possibly very similar to what you used for writing the original Caesar cipher.
```hs
import Data.Char

-- shifts each character of t by the ord of the corresponding character in k
-- wrapping around if it reaches 256
-- if k is shorter than t, k is repeated as many times as required
cipher :: [Char] -> [Char] -> [Char]
cipher t [] = error "key cannot be empty"
cipher t k  = go t k  where
  go []       _       = []
  go xs       []      = go xs k
  go (x:xs)   (y:ys)  = shift x y : go xs ys where
    shift x y = chr (rem (ord x + ord y) 256)


-- reverses the effect of cipher by unshifting each character in t
-- by the ord of the corresponding character in k
-- wrapping around if it goes below 0
-- if k is shorter than t, k is repeated as many times as required
uncipher :: [Char] -> [Char] -> [Char]
uncipher t [] = error "key cannot be empty"
uncipher t k  = go t k where
  go []       _       = []
  go xs       []      = go xs k
  go (x:xs)  (y:ys)  = unshift x y : go xs ys where
    unshift x y       = chr $ if num < 0 then num + 256 else num where
      num = ord x - ord y 
```
[Solution file](exercise.files/cipher_advanced.hs)


# As-patterns

_As-patterns_ in Haskell are a nifty way to be able to pattern match on part of something and still refer to the entire original value. Some examples:

```hs
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t
```

Here we pattern-matched on a tuple so we could get at the first value for printing, but used the `@` symbol to introduce a binding named `t` in order to refer to the whole tuple rather than just a part.
```REPL
Prelude> f (1, 2)
1
(1, 2)
```

We can use as-patterns with pattern matching on arbitrary data constructors, which includes lists:
```hs
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs
```
```REPL
λ> doubleUp []
[]
λ> doubleUp [1]
[1,1]
λ> doubleUp [1,2]
[1,1,2]
λ> doubleUp [1,2,3]
[1,1,2,3]
```
Use as-patterns in implementing the following functions:
1. This should return `True` if (and only if) all the values in the first list appear in the second list, though they need not be contiguous.

```hs
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
```
The following are examples of how this functions should work:
```REPL
λ> isSubSeqOf "blah" "blahwoot"
True
λ> isSubsecOf "blah" "wootblah"
True
λ> isSubsecOf "blah" "wboloath"
True
λ> isSubsecOf "blah" "wootblah"
False
λ> isSubsecOf "blah" "halbwoot"
False
λ> isSubsecOf "blah" "blawhoot"
True
```
Remember that the sub-sequence has to be in the original order!
```hs
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool 
isSubsequenceOf []            _   = True 
isSubsequenceOf _             []  = False 
isSubsequenceOf xs'@(x:xs) (y:ys)
  | x == y                        = isSubsequenceOf xs  ys
  | otherwise                     = isSubsequenceOf xs' ys
```
[Solution file](exercise.files/subsequence.hs)


2. Split a sentence into words, then tuple each word with the capitalized form of each.

```hs
capitalizeWords :: String
                -> [(String, String)]
```
```REPL
λ> capitalizeWords "hello world"
[("hello", "Hello"), (world", "World")]
```

```hs
import Data.Char


capitalizeWords :: String
                -> [(String, String)]
capitalizeWords text = map capitalize $ myWords text where
  capitalize []         = error "empty string"
  capitalize w@(w':ws)  = (w,toUpper w' : ws )


split :: Char -> String -> [String]
split d s = go d (dropWhile (==d) s) where
  go _ [] = []
  go d s = word : go d (dropWhile (==d) rest) where
    (word, rest) = break (==d) s

myWords :: String -> [String]
myWords = split ' '
```
[Soluttion file](exercise.files/capitalizeWords.hs)


## Language exercises

1. Write a function that capitalizes a word.

```hs
import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord word = toUpper (head word) : tail word
```
[Solution file](exercise.files/capitalizeWord.hs) 


2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord function.

```hs
import Data.Char (toUpper)


-- Capitalizes the first leters of each sentence in a paragraph.
-- Relies on the ending '.' to recognize a sentence.
capitalizeParagraph :: String -> String
capitalizeParagraph []    = error "empty text"
capitalizeParagraph text  = unwords $ go $ words $ capitalizeWord text where
  go []         = []
  go [x]     = [x]
  go (x:x':xs)
    | last x == '.' = x: capitalizeWord x' : go xs
    | otherwise     = x : x' : go xs


capitalizeWord :: String -> String
capitalizeWord word = toUpper (head word) : tail wordmport Data.Char (toUpper)
```
[Solution file](exercise.files/capitalizeParagraph.hs)

## Phone exercise

This exercise is made by geophf originally for 1HaskellADay.

Remember old-fashioned phone inputs for writing text where you had to press a button multiple times to get different letters to come up? You may still have to do this when you try to search for a movie to watch using your television remote control. You're going to write code to translate sequences of button presses into strings and vice versa.

So! Here is the layout of the phone:

```
------------------------------------
| 1       | 2 ABC      | 3 DEF      |
------------------------------------
| 1 GHI   | 2 JKL      | 3 MNO      |
------------------------------------
| 1 PQRS  | 2 TUV      | 3 WXYS     |
------------------------------------
| * ^     | 0 + _      | # . ,      |

```

Where star `*` gives you capitalization of the letter you're writing to your friends, and 0 is your space bar. To represent the digit itself, you press that digit once more than the letters it represents. If you press a button one more than is required to type the digit, it wraps around to the first letter. For example,
```
2     -> 'A'
22    -> 'B'
222   -> 'C'
2222  -> '2'
22222 -> 'A'
```
So on and so forth. We're going to kick this around.

1. Create a data structure that captures the phone layout above. The data structure should be able to express enough of how the layout works that you can use it to dictate the behavior of the functions in the following exercies.
```hs
type Digit    = Char
type Presses  = Int

data DaPhone  = DaPhone [Button]

data Button   = Button Digit [Char]

phone :: DaPhone
phone = DaPhone [ Button '1' "1"
                , Button '2' "2abc"
                , Button '3' "3def"
                , Button '4' "4ghi"
                , Button '5' "5jkl"
                , Button '6' "6mno"
                , Button '7' "7pqrs"
                , Button '8' "8tuv"
                , Button '9' "9wxyz"
                , Button '*' "*^"
                , Button '0' "0+_"
                , Button '#' "#.,"
```
[Solution file](exercise.files/phone.hs) 

2. Convert the following converstions into keypresses required to express them. We're going to suggest types and functions to fill in order to accomplish the goal, but they're not obligatory. If you want to do it differently, go right ahead.

```hs
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
  ```

  ```hs
  import Data.Char (isUpper, toUpper, toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (elemIndex)


type Digit    = Char
type Presses  = Int

newtype DaPhone  = DaPhone [Button]

data Button   = Button Digit [Char]

-- represents an old school phone keyboard
phone :: DaPhone
phone = DaPhone [ Button '1' "1"
                , Button '2' "2abc"
                , Button '3' "3def"
                , Button '4' "4ghi"
                , Button '5' "5jkl"
                , Button '6' "6mno"
                , Button '7' "7pqrs"
                , Button '8' "8tuv"
                , Button '9' "9wxyz"
                , Button '*' "*^"
                , Button '0' "0+_ "
                , Button '#' "#.,"
                ] 

-- calculates which button how many times must be pressed
-- to produce a specific character
reverseTaps :: DaPhone
            -> Char 
            -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) c
  | isUpper c = ('*', 1) : [charToTap buttons $ toLower c]
  | otherwise = [charToTap buttons c]
  where
      charToTap ((Button d s):bs) c
        | c == '1'        = ('1', 1)
        | isNothing num   = charToTap bs c
        | otherwise       = (d, if fromJust num == 0 then length s else fromJust num)
        where
          num = elemIndex c s
      charToTap [] _                = error ("where phone? or what char is" ++ [c] ++ "?")


-- produces the sequence of digits in the form of (digit, #press)
-- that must be pressed to produce a specific message
cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps


convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
  ```
  [Solution file](exercise.files/phone.hs)


3. How many times do digits need to be pressed for each message?

```hs
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd
```
[Solution file](exercise.files/phone.hs)


4. What was the most popular letter for each message? What was it's cost? You'll want to combine `revereseTaps` and `fingerTaps` to figure out what it cost in taps. `reverseTaps` is a list because you need to press a different button in order to get capitals.

```hs
-- calculates how many key presses are
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd


-- returns the letter that's most repeated in a string
-- ignores case
mostPopularLetter :: String -> Char 
mostPopularLetter text = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ freqLetter text where
  freqLetter :: String -> [(Char, Int)]
  freqLetter text = [(c, length $ filter (\x -> toLower x==c) text) | c <- ['a'..'z']]

-- calculates the cost (num of required presses)
-- of the most frequent char in a string
costOfMostPopularChar :: String -> Presses
costOfMostPopularChar = fingerTaps . reverseTaps phone . mostPopularLetter
```
[Solution file](exercise.files/phone.hs)

5. What was the most popular letter overall? What was the most popular word?

```hs
-- returns the letter that's most repeated in list of strings
-- ignores case
coolestLtr :: [String] -> Char
--            take the first element of the tuple
coolestLtr  = fst . 
--            get the maximum tuple comparing the second elements of them
              maximumBy (\(_,x) (_,y) -> compare x y) . 
--            create a list of tuples from a Map
              Map.toAscList . 
--            create a Map out of [(Char, Int)] tuples adding the Ints
              Map.fromListWith (+) . 
--            apply freqLetter to a [String] and concatenate the results
              concatMap freqLetter 


-- returns the word that's most repeated in a list of strings
-- ignores case
coolestWord :: [String] -> String
--                    take the first element of the tuple
coolestWord strings = fst .
--                    get the maximum tuple comparing the second elements of them 
                      maximumBy (comparing snd) .
--                    create a list of tuples from a Map 
                      Map.toAscList .
--                    create a Map out of [(String, Int)] tuples adding the Ints 
                      Map.fromListWith (+) $
--                    create lower case (word, 1) for each word in all the strings and concat them into one list
                      [(map toLower w, 1) | w <- concatMap words strings
```
[Solutin file](exercise.files/phone.hs)


## Hutton's Razor

Hutton's Razor is a very simple expression language that expresses integer literals and addition of values in that expression language. The "trick" to it is that it's recursive and the two expressions you're summing together could be literals or themselves further addition operations. This sort of datatype is stereotypical of expression languages used to motivate ideas in research papaers and functional pearls. Evaluating or folding a datatype is also in some sense what you're doing most of the time while programming anyway.

1. Your first task is to write the `eval` function which reduces an expression to a final sum.

```hs
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval = error "implement it!"
```

Example of expected output:
```REPL
λ> eval (Add (Lit 1) (Lit 9001))
9002
```

```hs
data Expr =
    Lit Integer
  | Add Expr Expr
  

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add exp1 exp2) = eval exp1 + eval exp2
```
[Solution file](exercise.files/huttonRazor.hs)

2. Write a printer for the expressions.

```hs
printExpr :: Expr -> String
printExpr = undefined
```
Expected output:
```REPL
λ> printExpr (Add (Lit 1) (Lit 9001))
"1 + 9001"
λ> a1 = Add (Lit 9001) (Lit 1)
λ> a2 = Add a1 (Lit 20001)
λ> a3 = Add (Lit 1) a2
λ> printExpr a3
"1 + 9001 + 1 + 20001"
```

```hs
-- prints the mathematical expression that Expr represents
printExpr :: Expr -> String 
printExpr (Lit int) = show int
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
```
[Solution file](exercise.files/huttonRazor.hs)