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

6.

```haskell
data Which a =
    ThisOne a
  | ThatOne a
```

**Answer:**

```haskell
data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') =
    a == a'
  (==) (ThatOne a) (ThatOne a') =
    a == a'
  (==) _ _ = False
```

7.

```haskell
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a)
       (Hello a') =
         a == a'
  (==) (Goodbye b)
       (Goodbye b') =
         b == b'
  (==) _ _ = False
```

## Tuple experiment

Look at the types given for `quotRem` and `divMod`. What do you think those functions do? Test your hypotheses by playing with them in the REPL. We've given you a sample to start with below:

`ones x = snd (divMod x 10)`

**Answer:** `ones` takes x and integer divides it by 10. `divMod` returns a tuple where the first element is the quotient and the second element is the remainder. The `snd` function returns the second element of the tuple which is the remainer.

## Will The Work?

Take a look at teh following code examples and try to decide if they will work, what result they will return if they do, and why or why not (be sure, as always, to test them in your REPL once you have decided on your answer):

1.

```haskell
max (length [1, 2, 3])
    (length [8, 9, 10, 11, 12])
```

**Answer:** Yes, this will work and the result will be `5`. The signature for `max` is `max :: (Ord a) => a -> a -> a` and the signature of `length` is `length :: [a] -> Int`. This will render the concrete signature of `max` into `max :: Int -> Int -> Int`. Evaluating the expression give `max 3 5 = 5`.

2.

```haskell
compare (3 * 4) (3 * 5)
```

**Answer:** Yes, this will work and the result will be `LT`. The signature for `compare` is `compare :: (Ord a) => a -> a -> Ordering`. This means that `compare` takes 2 arguments of any type that has an instance for `Ord` and returns a value of type `Ordering`. The `Ordering` type has 3 constructors `LT`, `GT`, and `EQ`. The input arguments will render the concrete signature of `compare` to `compare :: Int -> Int -> Ordering`

3.

```haskell
compare "Julie" True
```

**Answer:** No, this will not work. `compare` has the type `compare :: (Ord a) => a -> a -> Ordering`. Notice that both parameters of `compare` _must_ have the same type. Even when both `[Char]` and `Bool` have an instance of `Ord` there is no way the compiler knows how to compare them with one another.
