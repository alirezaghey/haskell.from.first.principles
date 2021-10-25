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


## Constant Instance

Write an `Applicative` instance for `Constant`.
```hs
newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap = undefined

instance Monoid a
    => Applicative (Constant a) where
  pure = undefined
  (<*>) = undefined
```

## Fixer Upper

Given the function and values provided, use `(<$>)` from `Functor`, `(<*>) and `pure` from the `Applicative` typeclass to fill in missing bits of the broken code to make it work.

```hs
const <$> Just "Hello" <*> "World"
```
**Answer:**
```hs
const <$> Just "Hello" <*> pure "World"
```

```hs
(,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
```

**Answer:**
```hs
pure (,,,) <*> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
```

## List Applicative Exercise

Implement the list `Applicative`. Writing a minimally complete `Applicative` instance calls for writing the definitions of both `pure` and `<*>`. We're going to provide a hint as well. Use the _checkers_ library to validate your `Applicative` instance.

```hs
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
```

Remember what you wrote for the list `Functor`:

```hs
instance Functor List where
  fmap = undefined
```

Writing the list `Applicative` is similar.

```hs
instance Applicative List where
  pure = undefined
  (<*>) = undefined
```
Expected result:
```
λ> f = Cons (+1) (Cons (*2) Nil)
λ> v = Cons 1 (Cons 2 Nil)
λ> f <*> v
Cons 2 (Cons 3 (Cons 2 (Const 4 Nil)))
```
In case you get stuck, use the following functions and hints.

```hs
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> c) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatmap :: (a - List b)
        -> List a
        -> List b
flatMap f as = undefined
```

Use the above and try using `flatmap` and `fmap` without explicitly pattern matching on cons cells. You'll still need to handle the `Nil` cases.

`flatMap` is less strange than it would initially seem. It's basically `fmap` then smush.

```
λ> fmap (\x -> [x, 9]) [1,2,3]
[[1,9], [2,9], [3,9]]

λ> toMyList = foldr Cons Nil
λ> xs = toMyList [1,2,3]
λ> c = Cons

λ> f x = x `c` (9 `c` Nil)
λ> flatMap f x
Cons 1 (Cons 9 (Cons 2
        Cons 9 (Cons 3 (Cons 9 Nil))))
```

`Applicative` instances, unlike `Functors`, are not guaranteed to have a unique implementation for a given datatype.

[Solution file (can be run as a standalone script)](exercise.files/listApplicative.hs)
