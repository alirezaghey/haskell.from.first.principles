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

[Solution file](./exercise.files/casePractice.hs)

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

## Artful Dodgy

Given the following definitions tell us what values resutls from further applications. When you've written down at least some of the answers and think you know what's what, type the definitions into a file and load them in GHCi to test your answers.

```haskell
-- Types not provided,
-- try filling them in yoursefl.

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
```

```haskell
-- above definitions with types

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
```

1. For example, given the expression `dodgy 1 0`, what do you think will happen if we evaluate it? If you put the definitions in a file and load them in GHCi, you can do the following to see the result.

```REPL
> dodgy 1 0
1
```

Now attempt to determine what the following expressions reduce to. Do it in your head, verify in your REPL after you think you have an answer.

2. `dodgy 1 1`
   <br>**Answer:** `11`

3. `dodgy 2 2`
   <br>**Answer:** `22`

4. `dodgy 1 2`
   <br>**Answer:** `21`

5. `dodgy 2 1`
   <br>**Answer:** `12`

6. `oneIsOne 1 <br>**Answer:** `11`

7. `oneIsOne 2`
   <br>**Answer:** `21`

8. `oneIsTwo 1`
   <br>**Answer:** `21`

9. `oneIsTwo 2`
   <br>**Answer:** `22`

10. `oneIsOne 3`
    <br>**Answer:** `31`

11. `oneIsTwo 3`
    <br>**Answer:** `23`

## Guard Duty

```haskell
avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
```

1. It is probably clear to you why you wouldn't put an `otherwise` in your top-most guard, but try it with `avgGrade` anyway and see what happens. It'll be more clear if you rewrite it as an `otherwise` match: `| otherwise = 'F'`. What happens now if you pass a `90` as an argument? `75`? `60`?
   <br>**Answer:** Since `otherwise` is always `True`, this branch of the guard will always hit and the function will return `F` in all the cases. $
2. What happens if you take `avgGrade` as it is written and reorder the guards? Does it still typecheck and work the same? Try moving `| y >= 0.7 = 'C'` and passing it the argument 90, which should be an `A`. Does it return an `A`?
   <br>**Answer:** If we reverse the order of the guards of `avgGrade`, the function will return `F` if `y` is less than `0.59` and a `D` in all the other cases because `y >= 0.59` is the second guard and it will always be true.
3. The following function returns:

```haskell
pal xs
    | xs == reverse xs = True
    | otherwise        = False
```

a. `xs` written backwards when it's True.
<br>b. `True` when `xs` is a palindrome.
<br>c. `False` when `xs` is a palindrome.
<br>d. `False` when xs is reversed.
<br>**Answer:** b

4. What types of arguments can `pal` take? **Answer:** `pal :: Eq a => [a] -> Bool`
5. What is the type of the function `pal`? **Answer:** `pal :: Eq a => [a] -> Bool`
6. The following function returns:

```haskell
numbers x
    | x < 0   = -1
    | x == 0  = 0
    | x > 0   = 1
```

a. the value of its argument plus or minus 1
<br>b. the negation of its argument
<br>c. an indication of whether its argument is a positive or negative number or zero
<br>d. binary machine language
<br>**Answer:** c

7. What types of arguments can `numbers` take? **Answer:** `numbers :: (Ord a, Num a, Num p) => a -> p`
8. What is the type of the function `numbers`? **Answer:** `numbers :: (Ord a, Num a, Num p) => a -> p`

## Pointfree style and function composition

[Example file](exercise.files/arith2.hs)

# Chapter Exercises

## Multiple choice

1. A polymorphic function

- [ ] a. changes things into sheep when invoked
- [ ] b. has multiple arguments
- [ ] c. has a aconcrete type
- [x] d. may resolve to values of different types, depending on inputs

2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composition `g . f` has the type:

- [ ] a. `Char -> String`
- [x] b. `Char -> [String]`
- [ ] c. `[[String]]`
- [ ] d. `Char -> String -> [String]`

3. A function `f` hs the type `Ord a = a -> a -> Bool` and we apply it to one numeric value. What is the type now?

- [ ] a. `Ord a => a -> Bool`
- [ ] b. `Num -> Num -> Bool`
- [ ] c. `Or a => a -> a -> Integer`
- [x] d. `(Ord a, Num a) => a -> Bool`

4. A function with the type `(a -> b) -> c`:

- [ ] a. requires values of three different types
- [x] b. is a higher-order function
- [ ] c. must take a tuple as its first argument
- [ ] d. has its parameters in alphabetical order

5. Given the following definition of `f`, what is the type of `f True`?

```hs
f :: a-> a
f x = x
```

- [x] `f True :: Bool`
- [ ] `f True :: String`
- [ ] `f True :: Bool -> Bool`
- [ ] `f True :: a`

## Let's write code

1. The following function returns the tens digit of an integral argument.

```hs
tensDigit :: Integral a => a -> a
tensDigit x = d
   where xLast = x `div` 10
         d     = xLast `mod` 10
```

a. First rewrite it using divMod.

**Answer:**

```hs
tensDigit :: Integral a => a -> a
tensDigit x = d
   where (xLast, _) = divMod x 10
         (_, d) = divMod xLast 10
```

b. Does the `divMod` version have the same type as the original version?
<br>**Answer:** Sure
<br>c. Next, let's change it so that we're getting the hundreds digit instead. You could start it like this (though that may not be the only possibility):

```hs
hunsD x = d2
   where d = undefined
```

**Answer:**

```hs
hunsD x = d
   where (xLast, _) = divMod x 100
         (_, d) = divMod xLast 10
```
