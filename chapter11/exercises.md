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
```