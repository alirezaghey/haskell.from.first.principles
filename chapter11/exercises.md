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