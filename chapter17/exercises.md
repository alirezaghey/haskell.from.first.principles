# Solutions to problems of chapter 16

## Exercises: Lookups

In the following exercises you will need to use the following terms to make the expressions typecheck:

```
pure
(<$>) -- or fmap
(<*>)
```

1. Make the following expression typecheck.

```hs
added :: Maybe Integer
added = (+3) (lookup 3 $ zip [1,2,3] [4,5,6])
```

**Answer:**
```hs
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])
```

2. Make the following expression typecheck.

```hs
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) y z
```

**Answer:**
```hs
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
```

3. Make the following expressions typecheck.

```hs
import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y :: Maybe Int
y = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' x y
```

**Answer:**
```hs
import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y :: Maybe Int
y = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y
```

4. Make the following expressions typecheck.

```hs
xs = [1,2,3]
ys = [4,5,6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum $ (,) x y
```

**Answer:**
```hs
xs = [1,2,3]
ys = [4,5,6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y
```

## Identity Instance

Write an `Applicative instance for Identity.

```hs
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  (<*>) = undefined
```
[Solution file](exercise.files/identityApplicative.hs)