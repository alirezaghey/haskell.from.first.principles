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
