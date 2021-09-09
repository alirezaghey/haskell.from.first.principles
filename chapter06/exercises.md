# Solutions to problems of chapter 6

## Eq Instances

Write the `Eq` instance for the datatype provided.

_Note:_ All the solutions to this exercise are in [exercise.files/EqInstancesExer.hs](exercise.files/EqInstancesExer.hs). In addition to the `prefix` implementation shown here, the file contains `infix` and `deriving` solutions too.

1. It's not a type, we're just being cute with the name.

```haskell
data TisAnInteger =
  TisAn Integer
```

**Answer:**

```haskell
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int)
       (TisAn int') =
     int == int'
```

2.

```haskell
data TwoIntegers =
    Two Integer Integer
```

**Answer:**

```haskell
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2)
       (Two int1' int2') =
    int1 == int1' && int2 == int2'
```

3.

```haskell
data StringOrInt =
    TisAnInt Int
  | TisAString String
```

**Answer:**

```haskell
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int)
       (TisAnInt int') =
         int == int'
  (==) (TisAString str)
       (TisAString str') =
         str == str'
  (==) _ _ = False
```
