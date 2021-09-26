module Vehicles where

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
