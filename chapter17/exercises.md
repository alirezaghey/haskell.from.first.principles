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

## ZipList Applicative Exercise

Implement the `ZipList Applicative`. Use the _checkers_ library to validate your `Applicative` instance. We're going to provide the `EqPropr` instance and explain the weirdness in a moment.

```hs
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap = undefined

instance Applicative List where
  pure = undefined
  (<*>) = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = undefined
  (<*>) = undefined
```

The idea is to align a list of functions with a list of values and apply the first function to the first value and so on. The instance should work with infinite lists. Some examples:

```
λ> zl' = ZipList'
λ> z = zl' [(+9), (*2), (+8)]
λ> z' = zl' [1..3]
λ> z <*> z'
ZipList' [10,4,11]
λ> z' = zl' (repeat 1)
λ> z <*> z'
ZipList' [10,2,9]
```

Note that the second `z'` was an infinite list. Check `Prelude` for functions that can give you what you need. One starts with the letter `z`, the other with the letter `r`. You’re looking for inspiration from these functions, not to be able to directly
reuse them as you’re using a custom List type, not the provided `Prelude` list type.

Explaining and justifying the weird `EqProp`:

The good news is it’s `EqProp` that has the weird "check only the first 3,000 values"
semantics instead of making the `Eq` instance weird. The bad news is this is a byproduct of testing for equality between infinite lists... that is, you can’t. If you use a typical `EqProp` instance, the test for homomorphism in your `Applicative` instance will
chase the infinite lists forever. Since `QuickCheck` is already an exercise in "good enough" validity checking, we could choose to feel justified in this. If you don’t believe us try running the following in your `REPL`:

```
repeat 1 == repeat 1
```
[Solution file (can be run as a script)](exercise.files/zipListApplicative.hs)

## Variations on Either

`Validation` has the same representation as `Either`, but it can be different. The `Functor` will behave the same, but the `Applicativbe` will be different. Use the _checkers_ library for testing.

```hs
data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap = undefined


-- This is different
instance Monoid e => Applicative (Validation e)
  pure = undefined
  (<*>) = undefined
```
[Solution file (can be run as a script)](exercise.files/validationApplicative.hs)


# Chapter Exercises

Given a type that has an instance of `Applicative`, specialize the types of the methods. Test your specialization in the `REPL`. One way to do this is to bind `aliases` of the typeclass methods to more concrete types that have the type we told you to fill in.

1. For the followoing type:

```hs
-- Type
[]

-- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b
```
**Answer:**
```hs
pure @[] :: a -> [a]
(<*>) @[] :: [(a -> b)] -> [a] -> [b]
```

2. For the following type:

```hs
-- Type
IO

-- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

**Answer:**
```hs
pure @IO :: a -> IO a
(<*>) @IO :: IO (a -> b) -> IO a -> IO b
```
3. For the following type:

```hs
-- Type
(,) a

-- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

**Answer:**
```hs
pure @((,) l):: a -> (l, a)
(<*>) @((,) l):: (l, a -> b) -> (l, a) -> (l, b)
```

4. For the following type:

```hs
-- Type
(->) e

-- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b

**Answer:**
```hs
pure @((->) e) :: a -> (e -> a)
(<*>) @((->) e) :: (e -> a -> b) -> (e -> a) -> (e -> b)
```