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

4.

```haskell
data Pair a =
  Pair a a
```

**Answer:**

```haskell
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2)
       (Pair a1' a2') =
         a1 == a1' && a2 == a2'
```

5.

```haskell
data Tuple a b =
  Tuple a b
```

**Answer:**

```haskell
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b)
       (Tuple a' b') =
         a == a' && b == b'
```
