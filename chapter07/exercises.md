# Solutions to Problmes of Chapter 6

## Grab Bag

Note the following exercises are from source code files, not written for use directly in the REPL. Of cource, you can change them to test directly in the REPL if you prefer.

1. Which (two or more) of the following are equivalent?
   <br>a. `mTh x y z = x * y * z`
   <br>b. `mTh x y = \z -> x * y * z`
   <br>c. `mTh x = \y -> \z -> x * y * z`
   <br>d. `mTh = \x -> \y -> \z -> x * y * z`
   <br>**Answer:** All of the above are equivalent.

2. The type of `mTh` (above) is `Num a => a -> a -> a -> a`. What is the type of `mTh 3`?
   <br>a. `Integer -> Integer -> Integer`
   <br>b. `Num a => a -> a -> a -> a`
   <br>c. `Num a => a -> a`
   <br>d. `Num a => a -> a -> a`
   <br>**Answer:** d
3. Next, we'll practice writings anonymous lambda syntax. For example, one could rewrite:

```haskell
addOne x = x + 1
```

Into:

```haskell
addOne = \x -> x + 1
```

Try to make it so it can still be loaded as a top-level definition by GHCi. This will make it easier to validate your answers.
<br> a. Rewrite the `f` function in the where clause.

```haskell
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1
```

**Answer:**

```haskell
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1
```

<br>b. Rewrite the following to use anonymous lambda syntax:

```haskell
addFive x y = (if x > y then y else x) + 5
```

**Answer:**

```haskell
addFive = \x -> \y -> (if x > y then y else x) + 5
```

<br>c. Rewrite the following so that it doesn't use anonymous lambda syntax:

```haskell
mflip f = \x -> \y -> f y x
```

**Answer:**

```haskell
mflip f x y = f y x
```

## Variety Pack

1. Given the following declarations

```haskell
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)
```

a. What is the type of `k`? **Answer:** `k :: (a, b) -> a`
<br>b. What is the type of `k2`? Is it the same type as `k1` or `k3`? **Answer:** The type of `k2` is `k2 :: String`. No, it isn't the same type as `k1` and `k3`.
<br>c. Of `k1`, `k2`, and `k3, which one will return the number `3`as a result? **Answer:**`k1`and`k3`will return the number`3` as result.

2. Fill in the definition of the following function:

```haskell
-- Remember: Tuples have the
--           same syntax for their
--           type constructors and
--           their data constructors.

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))

f = undefined
```

**Answer:**

```haskell
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))

f (a, _, c) (d, _, f) = ((a, d), (c, f))
```

## Case Practice

We're going to practice using case expressions by rewriting functions. Some of these functions you've seen in previous chapters (and some you'll see later usings different syntax yet again!), but you'll be writings new versions now. Please note these are all written as they would be in source code files and we recommend you write your answers in source files and then load into GHCi to check, rather than trying to do them directly into the REPL.

First, rewrite `if-then-else` expressions into case expressions.

1. The following should return `x` when `x` is greater than `y`.

```haskell
functionC x y = if (x > y) then x else y
```

**Answer**:

```haskell
function x y =
    case x > y of
      True -> x
      False -> y
```

2. The following will add `2` to even numbers and otherwise simply return the input value.

```haskell
ifEvenAdd2 n = if even n then (n+2) else n
```

**Answer:**

```haskell
ifEvenAdd2 n =
    case even n of
      True -> n+2
      False -> n
```

3. The following compares a value, `x`, to zero and returns an indicator for whether `x` is a positive number or negative number. But what if `x` is `0`? You may need to play with the `compare` function a bit to find what to do.

```haskell
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
```

**Answer:**

```haskell
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
```
