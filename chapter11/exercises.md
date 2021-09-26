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