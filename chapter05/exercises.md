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

## Write a type signature

For the following expressions, please add a type signature. You should be able to rely on GHCi type inference to check your work, although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc).

1. While we haven't fully explained this syntax yet, you've seen it in Chapter 2 and as a solution to an exercise in Chapter 4. This syntax is a way of destructuring a single element of a list by pattern matching.

```haskell
functionH ::
functionH (x:_) = x
```

**Answer:**

```haskell
functionH :: [a] -> a
functionH (x:_) = x
```

2.

```haskell
functionC ::
functionC x y = if (x > y) then True else False
```

**Answer:**

```haskell
functionC :: (Ord a) => a -> a -> Bool
```

3.

```haskell
functionS ::
functionS (x, y) = y
```

**Answer:**

```haskell
functionS :: (a, b) -> b
functionS (x, y) = y
```

## Given a type, write the function

You will be shown a type and a function that needs to be written. Use the information the type provides to determine what the function should do. We'll also tell you how many ways there are to write a function. Syntactically different but semantically equivalent implementations are not counted as being different. For example, writing a function one way then rewriting the semantically identical function but using anonymous lambda syntax does not count as two implementations.

To make things a little easier, we'll demonstrate how to solve this kind of exercise. Given:

```haskell
muFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = undefined
```

Talking through the above, we have a function that takes four arguments. The final result is a tuple with the type `(a, z)`. It turns out, the `c` argument is nowhere in our results and there's nothing to do with it, so we use the underscore to ignore that. We named the two function arguments by their types and pattern matched on the tuple argument. The only way to get the second value of the tuple from teh type `x` to the type `z` is to use _both_ of the functions furnished to us. If we tried the following:

```haskell
myFunc xToY yToZ _ (a, x) =
   (a, (xToY x))
```

We would get a type error that it expected the type `z` but the actual type was `y`. That's because we're on the right path, but not quite done yet! Accordingly, the following should typecheck:

```haskell
myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
   (a, (yToZ (xToY x)))
```

1. There is only one function definition that typechecks and doesn't go into an infinite loop when you run it.

```haskell
i :: a -> a
i = undefined
```

**Answer:**

```haskell
i :: a -> a
i x = x
```

2. There is only one version that works.

```haskell
c :: a -> b -> a
c = undefined
```

**Answer:**

```haskell
c :: a -> b -> a
c x y = x
```

3. Given alpha equivalence are `c''` and `c` (see above) the same thing?

```haskell
c'' :: b -> a -> b
c'' = undefined
```

**Answer:** Yes they are alpha equivalent.

```haskell
c'' :: b -> a -> b
c'' x y = x
```

4. There is only one version that works.
   ```haskell
   c' :: a -> b -> b
   c' = undefined
   ```

**Answer:**

```haskell
c' :: a -> b -> b
c' x y = y
```

5. There are multiple possibilities, at least two of which you've seen in previous chapters.
   ```haskell
   r :: [a] -> [a]
   r = undefined
   ```

**Answer:** Here are a few possible solutions

```haskell
r :: [a] -> [a]
r x = tail x
-- or
r x = x ++ x
-- or
r x = concat [x, x] -- can add as many xs you like
-- or
r x = reverse x
```

6. There is only one version that will typecheck.

```haskell
co :: (b -> c) -> (a -> b) -> a -> c
co = undefined
```

**Answer:**

```haskell
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a
```

7. One version will typecheck.

```haskell
a :: (a -> c) -> a -> a
a = undefined
```

**Answer:**

```haskell
a :: (a -> c) -> a -> a
a _ x = x
```

8. One version will typecheck.

```haskell
a' :: (a -> b) -> a -> b
a' = undefined
```

**Answer:**

```haskell
a' :: (a -> b) -> a -> b
a' aToB a = aToB a
```

## Fix it

Won't someone take pity on this poor broken code and fix it up? Be sure to check carefully for things like capitalization, parentheses, and indentation.

1.

```haskell
module sing where

fstString :: [Char] ++ [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x or sndString y
where x = "Singin"
      x = "Somewhere"
```

**Answer:** Solution file: [./exercise.files/sing.hs](exercise.files/sing.hs)

2. Now that it's fixed, make a minor change and make it sing the other song. If you're lucky, you'll end up with both songs stuck in your head!

**Answer:** Solution file: [./exercise.files/sing.hs](exercise.files/sing2.hs)

3.

```haskell
module Arith3Broken where

main :: IO ()
Main = do
   print 1 + 2
   putStrLn 10
   print (negate -1)
   print ((+) 0 blah)
   where blah = negate 1
```

**Answer:** Solution file: [./exercise.files/arith3broken_fixed.hs](exercise.files/arith3broken_fixed.hs)

## Type-Kwon-Do

The name is courtesy of Phillip Wright. Thank you for the idea!

The focus here is on manipulating terms in order to get the types to fit. This _sort_ of exercise is something you???ll encounter in writing real Haskell code, so the practice will make it easier to deal with when you get there. Practicing this will make you better at writing ordinary code as well.
We provide the types and bottomed out (declared as `undefined`) terms. _Bottom_ and _undefined_ will be explained in more detail later. The contents of the terms are irrelevant here. You???ll use only the declarations provided and what the Prelude provides
by default unless otherwise specified. Your goal is to make the `???`???d declaration pass the typechecker by modifying it alone.
Here???s a worked example for how we present these exercises and how you are expected to solve them. Given the following:

```haskell
data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g = ???
```

Here it???s _g_ that you???re supposed to implement; however, you can???t evaluate anything. You???re to only use type-checking and type-inference to validate your answers. Also note that we???re using a trick for defining datatypes which can be named in a type signature, but have no values. Here???s an example of a valid solution:

```haskell
g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)
```

The idea is to only fill in what we've marked with `???`.

_Not all terms will always be used in the intended solution for a problem._

1.

```haskell
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = ???
```

**Answer:**

```haskell
h :: Int -> Char
h i = g f i
```

2.

```haskell
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = ???
```

**Answer:**

```haskell
e :: A -> C
e a = w q a
-- we could also write 'e a = w $ q a' which would make it more readable
-- but syntax-wise the first solution is also correct
-- this is also valid for the previous problem
```

3.

```haskell
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform = ???
```

**Answer:**

```haskell
xform :: (X, Y) -> (Z, Z)
xform x y = (xz x, yz y)
-- we could also write 'xform x y = (yz y, xz x)'
-- this may evaluate to something else depending on implementation
-- but type-wise it is correct
```

4.

```haskell
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge = ???
```

**Answer:**

```haskell
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZTuple x = fst $ yToWZTuple $ xToY x
```
