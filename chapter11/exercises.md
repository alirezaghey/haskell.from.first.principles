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