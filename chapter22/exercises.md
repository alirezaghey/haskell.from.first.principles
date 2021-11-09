# Solutions to problems of chapter 21

## FAM for functions

The `Functor` instance for functions is function composition. Meaning:
```
f <$> g == fmap f g == f . g
```

The `Applicative` and `Monad` instances for functions passes incoming arguments to _all_ the involved functions in the `Applicative`/`Monad` chain. Meaning:
```
(+) <$> (*2) <*> (+10) == liftA2 (+) (*2) (+10)
-- above is a function that takes one Num and returns one Num.
-- incoming Num is passed to both (*2) and (+10)
-- the results of those functions is passed to (+)
```
For `Monad`s:
```
test = do
  a <- (+1)
  b <- (*10)
  return (a + b)
-- above is a function that takes one Num and returns one Num.
-- incoming Num is passed to both (+1) and (*10)
-- the results of those functions is passed to (+)
```
These examples are in [this file](exercise.files/famForFunctions.hs)


## Warming Up

Implement the undefined functions. `composed` and `fmapped` should have identical results.
```hs
import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = undefined

fmapped :: [Char] -> [Char]
fmapped = undefined
```
[Solution file](exercise.files/famForFuncsExercise.hs)

Now we want to return the results of `cap` and `rev` both as a tuple like the following:
```
λ> tupled "Julie"
("JULIE", "eiluJ")
-- or
λ> tupled' "Julie"
("eiluJ", "JULIE")
```
The signature is as follows:
```hs
tupled :: [Char] -> ([Char], [Char])
```
Write both a definition using the `Applicative` and `Monad` instances for functions.
[Solution file](exercise.files/famForFuncsExercise.hs)


## Exercise: Ask

Implement the following function

```hs
ask :: Reader a a
ask = Reader ???
```
**Answer:**
```hs
newtype Reader r a =
  Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id
```
[Solution file](exercise.files/ask.hs)


## Reading Comprehension

1. Write `liftA2` yourself.

```hs
myLiftA2 :: Applicative f =>
            (a -> b -> c)
            -> f a -> f b -> f c
myLiftA2 = undefined
```
**Answer:**
```hs
myLiftA2 :: Applicative f =>
            (a -> b -> c) ->
            f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b
```
[Solution file](exercise.files/myLiftA2.hs)

2. Write the following function.

```hs
asks :: (r -> a) -> Reader r a
asks f = Reader ???
```
**Answer:**
```hs
newtype Reader r a = Reader {runReader :: r -> a}

asks :: (r -> a) -> Reader r a
asks = Reader
```
[Solution file](exercise.files/myLiftA2.hs)

3. Implement the `Applicative` for `Reader`.

To write the `Applicative` instance for `Reader`, we'll use an extension called `InstanceSigs`. It's an extension we need in order to assert a type for the typeclass methods. You ordinarily cannot assert type signatures in instances. The compiler already knows the type of the functions, so it's not usually necessary to assert the types in instances anyway. We did this for the sake of clarity, to make the `Reader` type explicit in our signatures.

```hs
-- you'll need this pgrama
{-# LANGUAGE instanceSigs #-}

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ ???

(<*>) :: Reader r (a -> b)
      -> Reader r a
      -> Reader r a
(Reader rab) <*> (Reader ra) = Reader $ \r -> ???
```

Some instructions and hints.

  **a.** When writing the `pure` function for `Reader`, remember that what you're trying to construct is a function that takes a value of type _r_, which you know nothing about, and return a value of type _a_. Given that you're not really doing anything with _r_, there's really only one thing you can do.
  <br>**b.** We got the definition of the apply function started for you, we'll describe what you need to do and you write the code. If you unpack the type of `Reader`'s apply above, you get the following:

  ```hs
  <*> :: (r -> a -> b)
      -> (r -> a)
      -> (r -> b)

-- contrast this with tye type of fmap

fmap :: (a -> b)
    ->  (r -> a)
    ->  (r -> b)
```

So, what's the difference? The difference is that apply, unlike fmap, also takes an argument of type _r_. Make it so.

## Reader Monad

1. Implement the `Reader Monad`.

```hs
{-# LANGUAGE instanceSigs #-}

instance Monad (Reader r) where
  return = pure

(>>=) :: Reader r a
      -> (a -> Reader r b)
      -> Reader r b
(Reader ra) >>= aRb = Reader $ \r -> ???
```
**Answer:**
```hs
instance Monad (Reader r) where
  return = pure

(>>=) :: Reader r a
      -> (a -> Reader r b)
      -> Reader r b
(Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r
```
[Solution file](exercise.files/reader.hs)