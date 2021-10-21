# Solutions to problems of chapter 16

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What's the kind of _a_?

```hs
a -> a
```
**Answer:** The kind of `a` is `*`

2. What are the kinds of _b_ and _T_? (The _T_ is capitalized on purpose!)

```hs
a -> b a -> T (b a)
```

**Answer:** The kind of `b` is `* -> *` and the kind of `T` is also `* -> *`.

3. What's the kind of _c_?

```hs
c a b -> c b a
```

**Answer:** `c` is of kind `* -> * -> * `.


## Heavy Lifting

Add `fmap`, parentheses, and function composition to the expression as needed for the expression to typecheck and produce the expected result. It may not always need to go in the same place, so don't get complacent.

1. For the following statement:

```hs
a = (+1) $ read "[1]" :: [Int]
```
The expected result is:
```
λ> a
[2]
```

**Answer:**
```hs
a = fmap (+1) $ read "[1]" :: [Int]
```

2. For the following statement:

```hs
b = (++ "lol") (Just ["Hi,", "Hello"])
```

The expected result is:
```
λ> b
Just ["Hi,lol", "Hellolol"]
```

**Answer:**
```hs
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
```

3. For the following statement:

```hs
c = (*2) (\x -> x - 2)
```

The expected result is:
```
λ> c 1
-2
```

**Answer:**
```hs
c = fmap (*2) (\x -> x -2)
```

4. For the following statement:

```hs
d = ((return '1' ++) . show) (\x -> [x, 1..3])
```

The expected result is:
```
λ> d 0
"1[0,1,2,3]"
```

**Answer:**
```hs
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
```

5. For the following code snippet:

```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi
    in (*3) changed
```

The expected result is:
```
λ> e
3693
```

**Answer:**
```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
```

## Instances of Func

Implement `Functor` instances for the following datatypes. Use the `QuickCheck` properties we showed you to validate them.

1. For the following data type

```hs
newtype Identity a = Identity a
```
[Solution file](exercise.files/identityFunctor.hs)

2. For the following data type

```hs
data Pair a = Pair a a
```
[Solution file](exercise.files/pairFunctor.hs)

3. For the following data type

```hs
data Two a b = Two a b
```
[Solution file](exercise.files/twoFunctor.hs)

4. For the following data type

```hs
data Three a b c = Three a b c
```
[Solution file](exercise.files/threeFunctor.hs)

5. For the following data type

```hs
data Three' a b = Three' a b b
```
[Solution file](exercise.files/threePrimeFunctor.hs)


6. For the following data type

```hs
data Four a b c d = Four a b c d
```
[Solution file](exercise.files/fourFunctor.hs)

7. For the following data type

```hs
data Four' a b = Four' a a a b
```
[Solution file](exercise.files/fourPrimeFunctor.hs)


8. Can you implement a `Functor` instance for this type? Why? Why not?

```hs
data Trivial = Trivial
```
No, I can not. A `Functor` instance needs a type of kind `* -> *`  while `Trivial` has kind `*`.


## Possibly

Write a `Functor` instance for a datatype identical to `Maybe`. We'll use our own datatype because `Maybe` already has a `Functor` instance and we cannot make a duplicate one.

```hs
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)
```
[Solution file](exercise.files/possiblyFunctor.hs)


## Short Exercise

1. Write a `Functor` instance for a datatype identical to `Either`. We'll use our own datatype because `Either` has a `Functor` instance.

```hs
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
```
[Solution file](exercise.files/sumFunctor.hs)


2. Whi is a `Functor` instance that applies the function only to `First`, `Either`s `Left`, impossible?

`Functor` instance needs a type constructor that only lacks its last argument and will supply that argument based on the signature of the function that is applied to `fmap`. In case we wanted to apply that function to the first argument of `Either` we needed to provide its second argument on instance creation and leave the first one free for the function to `fmap` to be able to determine it. This is syntactically impossible or very awkward, at least. If we wanted to really do that, we would probably need to create another class than `Functor`.

# Chapter exercises

## Can we write a `Functor`?

Determine if a valid `Functor` can be written for the datatype provided.

1. For the following data type

```hs
data Bool =
  False | True
```

**Answer:** No, a `Functor` instance needs a data type of kind `* -> *` but `Bool` is of kind `*`.

2. For the following data type

```hs
data BoolAndSomethingElse a =
  False' a | True' a
```

**Answer:** Yes, a valid `Functor` can written for this data type as follows:
```hs
data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Show, Eq)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True' (f x)
```

3. For the following data type

```hs
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
```

**Answer:** Yes, a valid `Functor` can be written for this data type as follows:

```hs
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)
```

4. Use the kinds to guide you on this one, don't get too hung up on the details.

```hs
newtype Mu f = Inf {outF :: f (Mu f)}
```

**Answer:** No, a valid `Functor` can't be written for it. It has kind `(* -> *) -> *` which isn't right.

5. Again, follow the kinds and ignore the unfamiliar parts.

```hs
import GHC.Arr

data D =
  D (Array Word Word) Int Int
```

**Answer:** No, a valid `Functor` can't be written for it. It has kind `*` which isn't right.

## Rearange args to make `Functor` work

Rearrange the arguments to the type constructor of the datatype so the `Functor` instance works.

1. For the following data type

```hs
data Sum a b =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b
```

**Answer:**
```hs
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b
```

2. For the following data type

```hs
data Company a b c =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
```

**Answer:**
```hs
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
```

3. For the following data type

```hs
data More a b =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

**Answer:**
```hs
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

## Implementing `Functor` instances

Write `Functor` instances for the following datatypes.

1. For the following data type

```hs
data Quant a b =
    Finance
  | Desk a
  | Bloor b
```
[Solution file](exercise.files/quantFunctor.hs)

2. For the following data type

```hs
data K a b =
  K a
```
[Solution file](exercise.files/kFunctor.hs)


3. For the following data type

```hs
{-# LANGUAGE FlexibleInstance #-}

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a


-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap = undefined
```
[Solution file](exercise.files/flipFunctor.hs)

4. For the following data type

```hs
data EvilGoateeConst a b =
  GoatyConst b
```
It doesn't do anything interesting. If it works, you succeeded.
[Solution file](exercise.files/goatyConstFunctor.hs)

5. Do you need something extra to make the instance work?

```hs
data LiftItOut f a =
  LiftItOut (f a)
```
[Solution file](exercise.files/liftItOutFunctor.hs)