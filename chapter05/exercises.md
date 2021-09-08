# Solutions to problems of chapter 5

## Type Matching

Below you'll find a list of several standard functions we've talked about previously. Under that is a list of their type signatures. Mathch the function to its type signature.

1. Functions:
   <br>a. `not`
   <br>b. `length`
   <br>c. `concat`
   <br>d. `head`
   <br>e. `(<)`
2. Type signatures:
   <br>a. `_ :: [a] -> a`
   <br>b. `_ :: [[a]] -> [a]`
   <br>c. `_ :: Bool -> Bool`
   <br>d. `_ :: [a] -> Int`
   <br>e. `_ :: Ord a => a -> a -> Bool`

**Answers:** (a, c), (b, d), (c, b), (d, a), (e, e)

## Manual currying and un-currying

Load [curry_uncurry_anon.hs](./exercise.files/curry_uncurry_anon.hs) into GHCi and check that all the functions return the same result given equal input.

```REPL
Prelude> curriedFunction 10 True
815
Prelude> curriedFunction 20 False
31357
Prelude> uncurriedFunction (10, True)
815
Prelude> uncurriedFunction (20, False)
3157
Prelude> anonymous 10 True
815
Prelude> anonymous 20 False
3157
Prelude> anonNested 10 True
815
Prelude> anonNested 20 False
3157
```

## Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.

You can check your work in the REPL:

```REPL
Prelude> let f :: a -> a -> a -> a; f = undefined
Prelude> let x :: Char; x = undefined
Prelude> :t f x
```

It turns out that you can check the types of things that aren't implemented yet, so long as you give GHCi an `undefined` to bind the signature to.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char` then the type of `f x` is:
   <br> a. `Char -> Char -> Char`
   <br> b. `x -> x -> x -> x`
   <br> c. `a -> a -> a`
   <br> d. `a -> a -> a -> Char`
   <br>**Answer:** a

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is:
   <br> a. `String`
   <br> b. `Char -> String`
   <br> c. `Int`
   <br> d. `Char`
   <br>**Answer:** d

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2` is:
   <br> a. `Double`
   <br> b. `Integer`
   <br> c. `Integral b => b`
   <br> d. `Num b => b`
   <br> **Answer:** d

Note that because the type variables `a` and `b` are different, the compiler _must_ assume that the types could be different.

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1 (5.5 :: Double)` is:
   <br> a. `Integer`
   <br> b. `Fractional b => b`
   <br> c. `Double`
   <br> d. `Num b => b`
   <br> **Answer:** c

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" "has the word jackal in it"` is:
   <br> a. `[Char]`
   <br> b. `Eq b => b`
   <br> c. `b -> [Char]`
   <br> d. `b`
   <br> e. `Eq b => b -> [Char]`
   <br>**Answer:** a

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a` then the type of `jackal "keyboard"` is:
   <br> a. `b`
   <br> b. `Eq b => b`
   <br> c. `[Char]`
   <br> d. `b -> [Char]`
   <br> e. `Eq b => b -> [Char]`
   <br> **Answer:** e

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a` then the type of `kessel 1 2` is:
   <br> a. `Integer`
   <br> b. `Int`
   <br> c. `a`
   <br> d. `(Num a, Ord a) => a`
   <br> e. `Ord a => a`
   <br> f. `Num a => a`
   <br> **Answer:** d

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 (2 :: Integer)` is:
   <br> a. `(Num a, Ord a) => a`
   <br> b. `Int`
   <br> c. `a`
   <br> d. `Num a => a`
   <br> e. `Ord a => a`
   <br> f. `Integer`
   <br> **Answer:** a

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is:
   <br> a. `Num a => a`
   <br> b. `Ord a => a`
   <br> c. `Integer`
   <br> d. `(Num a, Ord a) => a`
   <br> e. `a`
   <br> **Answer:** c

## Parametricity

All you can do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.

1. Given the type `a -> a`, which is the type for `id`, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.
2. We can get a more comfortable appreciate of parametricity by looking at `a -> a -> a`. This hypothetical function `a -> a -> a` has two-and only two-implementations. Write both possible versions of `a -> a -> a`. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

**Answer:**

```haskell
f :: a -> a -> a
f x y = x
-- or
f x y = y
```

3. Implement `a -> b -> b`. How many implementations can it have? Does the behavior change when the types of `a` and `b` change?

**Answer:**

```haskell
f :: a -> b -> a
f x y = y
```

This is the only possible implementation. The behavior does not change when the types of `a` and/or `b` change.

## Apply Yourself

Look at these pairs of functions. One function is unapplied, so the compiler will infer maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. Figure out how the type would change and why, make a note of what you think the new inferred type would be and then check your work in GHCi.

1.

```haskell
-- Type signature of general function
(++) :: [a] -> [a] -> [a]

-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ " yo"
```

**Answer:** `(++) :: [Char] -> [Char] -> [Char]`

2.

```haskell
-- General function
(*) :: Num a => a -> a -> a

-- Applied to a value
myMult x = (x / 3) * 5
```

**Answer:** `(*) :: Fractional a => a -> a -> a`

3.

```haskell
take :: Int -> [a] -> [a]

myTake x = take x "hey you"
```

**Answer:** `take :: Int -> [Char] -> [Char]`

4.

```haskell
(>) :: Ord a => a -> a -> Bool

myCom x = x > (length [1..10])
```

**Answer:** `(>) :: Int -> Int -> Bool

5.

```haskell
(<) :: Ord a => a -> a -> Bool

myAlph x = x < 'z'
```

**Answer:** `(<) :: Char -> Char -> Bool`

# Chapter Exercises

## Multiple choice

1. A value of type `[a]` is:

- [ ] a. a list of alphabetic characters
- [ ] b. a list of lists
- [x] c. a list whose elements are all of some type `a`
- [ ] d. a list whose elements are all of different types

_Note:_ `[a]` could also be of the types described in `a` and `b` but it _does not have_ to be. In other words, the types described in `a` and `b` are subsets of `c`. `c` is the most polymorphic permissable type that still respects the constraints of `[a]`.

2. A function of type `[[a]] -> [a]` could:

- [x] a. take a list of strings as an argument
- [ ] b. transform a character into a string
- [ ] c. transform a string into a list of strings
- [ ] d. take two arguments

3. A function of type `[a] -> Int -> a`:

- [ ] a. takes one argument
- [x] b. returns one element of type `a` from a list
- [ ] c. must return an `Int` value
- [ ] d. is completely fictional

4. A function of type `(a, b) -> a`:

- [ ] a. takes a list argument and returns a `Char` value
- [ ] b. has zero arguments
- [x] c. takes a tuple argument and returns the first value
- [ ] d. requires that `a` and `b` be of different types

## Determine the type

For the following functions, determine the type of the specified value. We suggest you type them into a file and load the contents of the file in GHCi. In all likelihood, it initially will not have the polymorphic types you might expect due to the monomorphism restriction. That means that top-level declarations by default will have a concrete type if any can be determined. You can fix this by setting up your file like so:

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1
```

If you had not included the `NoMonomorphismRestriction` extension, `example` would have had the type `Integer` instead of `Num a => a`. Do your best to determine the _most_ polymorphic type an expression could have in the following exercises.

1. All function applications return a value. Determin the value returned by these function applications and the type of that value.
   <br> a. `(* 9) 6`
   <br> **Answer:** value: `36`, type: `Num a => a`
   <br> b. `head [(0, "doge"), (1, "kitteh")]`
   <br> **Answer:** value: `(0, "doge")`, type: `Num a => (a, [Char])`
   <br> c. `head [(0 :: Integer, "doge"), (1, "kitteh")]`
   <br> **Answer:** value: `(0, "doge")`, type: `(Integer, [Char])`
   <br> d. `if False then True else False`
   <br> **Answer:** value: `False`, type: `Bool`
   <br> e. `length [1, 2, 3, 4, 5]`
   <br> **Answer:** value: `5`, type: `Int`
   <br> f. `(length [1, 2, 3, 4]) > (length "TACOCAT")`
   <br> **Answer:** value: `False`, type: `Bool`

2. Given:

```haskell
x = 5
y = x + 5
w = y * 10
```

What is the type of `w`?
<br> **Answer:** `Num a => a`

3. Given:

```haskell
x = 5
y = x + 5
z y = y * 10
```

What is the type of `z`?
<br>**Answer:** `z :: Num a => a -> a`

4. Given:

```haskell
x = 5
y = x + 5
f = 4 / y
```

What is the type of `f`?
<br>**Answer:** `f :: Fractional a => a`

5. Given:

```haskell
x = "Julie"
y = " <3"
z = "Haskell"
f = x ++ y ++ z
```

What is the type of `f`?
<br> **Answer:** `f :: [Char]`

## Does it compile?

For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.

1.

```haskell
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
```

<br>**Answer:** The second line `wahoo = bigNum $ 10` throws an error. It is not clear what operation we intend to do with the two numbers.

2.

```haskell
x = print
y = print "woohoo!"
z = x "hello world"
```

<br>**Answer:** None of the lines is syntactically incorrect.

3.

```haskell
a = (+)
b = 5
c = b 10
d = c 200
```

<br>**Answer:** Lines 3 and 4 are problematic because 2 numbers are introduced without any operator or function. We can rewrite the code as follows:

```haskell
a = (+)
b = 5
c = a b 10 -- c evaluates to (+) b 10 = 5 + 10
d = a c 200 -- d evaluates to (+) c 200 = (5 + 10) + 200
```

4.

```haskell
a = 12 + b
b = 10000 * c
```

<br>**Answer:** `c` is not in scope. Adding it will solve the problem. Keep in mind that for it to run in GHCi, you need to arrange the order such that every introduced variable is defined previously.

```haskell
a = 12 + b
b = 10000 * c
c = 1
```

## Type variable or specific type constructor?

You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type constructor.

1.

```haskell
f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
```

- ([0]), constrained polymorphic `Num`
- ([1]), fully polymorphic
- ([2],[3]), concrete type constructor

2.

```haskell
f :: zed -> Zed -> Blah
--   [0]    [1]    [2]
```

- ([0]), fully polymorphic
- ([1], [2]), concrete type constructor

3.

```haskell
f :: Enum b => a -> b -> C
--            [0]  [1]  [2]
```

- ([0]), fully polymorphic
- ([1]), constrained polymorphic `Enum`
- ([2]), concrete type constructor

4.

```haskell
f :: f -> g -> C
--  [0]  [1]  [2]
```

- ([0], [1]), fully polymorphic
- ([2]), concrete type constructor
