# Solutions to problems of chapter 15

## Optional Monoid

Write the `Monoid` instance for your `Maybe` type renamed to `Optional`.'

```hs
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty  = undefined
  mappend = undefined
```

```hs
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a
      => Semigroup (Optional a) where
  (Only x)  <> (Only y) = Only (x <> y)
  Nada      <> (Only y) = Only y
  (Only x)  <> Nada     = Only x
  Nada      <> Nada     = Nada

instance Monoid a
      => Monoid (Optional a) where
  mempty      = Nada
  mappend x y = x <> y
```
[Solution file](exercise.files/optional-monoid.hs)


## Madness

You may have seen mad libs before. The idea is to take a template of phrases, fill them in with blindly selected categories of words, and see if saying the final version is amusing.

Using a lightly edited example from the Wikipedia article on Mad Libs:
```
___________! he said ______ as he<br>
exclamation          adverb
jumped into his car ____ and drove<br>
                    noun
off with his _________ wife."
             adjective
````

We can make this into a function, like the following:

```hs
import Data.Monoid

type Verb           = String
type Adjective      = String
type Adverb         = String
type Noun           = String
type Exclamation    = String

madlibbin'  :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbin' e adv noun adj =
  e     <> "! he said " <>
  adv   <> " as he jumped into his car " <>
  noun  <> " and drove off with his " <>
  adj   <> " wife."
```

Now you're going to refactor this code a bit! Rewrite it using `mconcat`.
[Solution file](exercise.files/madness.hs)


## Maybe Another Monoid

Write a `Monoid` instance for a `Maybe` type which doesn't require a `Monoid` for the contents. Reuse the `Monoid` law `QuickCheck` properties and use them to validate the instance.

Don't forget to write an `Arbitrary` instance for `First'`. We won't always stub that out explicitly for you. We suggest learning how to use the `frequency` function from `QuickCheck` for `First'`s instance.

```hs
newtype First' a =
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = undefined
  mappend = undefined

firstMappend  :: First' a
              -> First' a
              -> First' a
firstMappend  = mappend

type FirstMappend =
      First' String
  ->  First' String
  ->  First' String
  ->  Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
```
[Solution file (can be run directly from terminal with cabal)](exercise.files/maybe-another-monoid.hs)

# Chapter exercises

## Semigroup exercises

Given a datatype, implement the `Semigroup` instance. Add `Semigroup` constraints to type variables where needed. Use the `Semigroup` class from the `semigroups` library (or from base if you are on GHC 8) or write your own. When we use `(<>)`, we mean the infix `mappend` from the `Semigroup` typeclass.

**Note:** We're not always going to derive every instance you may want or need in the datatypes we provide for exercises. We expect you to know what you need and to take care of it yourself by this point.

1. Validate _all_ of your instances with `QuickCheck`. Since `Semigroup`'s only law is associativity, that's the only property you need to reuse. Keep in mind that you'll potentially need to import the modules for `Monoid` and `Semigroup` and to avoid naming conflicts for the `(<>)` depending on your version of GHC.

```hs
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = undefined

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc  :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc x y z =
  (x <> y) <> z == x <> (y <> z)

type TrivialAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: TrivAssoc)
```
[Solution file (can be run directly from terminal with cabal)](exercise.files/trivial.hs)

2. Validate associativity with `QuickCheck`

```hs
newtype Identity a = Identity a
```
[Solution file (can be run directly from terminal with cabal)](exercise.files/identity.hs)

3. Validate associativity with `QuickCheck`

```hs
data Two a b = Two a b
```
Hint: Ask for another `Semigroup` instance.
[Solution file (can be run with cabal)](exercise.files/two.hs)

4. Validate associativity with `QuickCheck`

```hs
data Three a b c = Three a b c
```
[Solution file (can be run with cabal)](exercise.files/three.hs)

5. Validate associativity with `QuickCheck`

```hs
data Four a b c d = Four a b c d
```
[Solution file (can be run with cabal)](exercise.files/four.hs)

6. Validate associativity with  `QuickCheck`

```hs
newtype BoolConj =
  BoolConj Bool
```
What it should do:

```
??> (BoolConj True) <> (BoolConj True)
BoolConj True
??> (BoolConj True) <> (BoolConj False)
BoolConj False
```
[Solution file (can be run with cabal)](exercise.files/boolConj.hs)

7. Validate associativity with `QuickCheck`

```hs
newtype BoolDisj =
  BoolDisj Bool
```
What it should do:
```
??> (BoolDisj True) <> (BoolDisj True)
BoolDisj True
??> (BoolDisj True) <> (BoolDisj False)
BoolDisj False
```
[Solution file (can be run with cabal)](exercise.files/boolDisj.hs)

8. Validate associativity with `QuickCheck`

```hs
data Or a b =
    Fst a
  | Snd b
```
The `Semigroup` for `Or` should have the following behavior. We can think of this as having a "sticky" `Snd` value where it'll hold onto the first `Snd` value when and if one is passed as an argument. This is similar to the `First'` `Monoid` you wrote earlier.

```
??> Fst 1 <> Snd 2
Snd 2
??> Fst 1 <> Fst 2
Fst 2
??> Snd 1 <> Fst 2
Snd 1
??> Snd 1 <> Snd 2
Snd 1
```
[Solution file (can be run with cabal)](exercise.files/or.hs)

9. Validate associativity with `QuickCheck`

```hs
newtype Combine a b =
  Combine {unCombine :: (a -> b)}
```
What it should do:

```
??> f = Combine $ \n -> Sum (n + 1)
??> g = Combine $ \n -> Sum (n - 1)
??> unCombine (f <> g) $ 0
Sum {getSum = 0}
??> unCombine (f <> g) $ 1
Sum {getSum = 2}
??> unCombine (f <> f) $ 1
Sum {getSum = 4}
??> unCombine (g <> f) $ 1
Sum {getSum = 2}
```
Hint: This function will eventually be applied to a single value of type `a`. But you'll have multiple functions that can produce a value of type `b`. How do we combine multiple values so we have a single `b`? This one will probably be tricky! Remember that the type of the value inside of `Combine` is that of a _function_. The type of functions should already have an `Arbitrary` instance that you can reuse for testing this instance.

[Solution file](exercise.files/combine.hs)

10. Validate associativity with `QuickCheck`.

```hs
newtype Comp a =
  Comp {unComp :: (a -> a)}
```

Hint: We can do something that seems a little more specific and natural to functions now that the input and output types are the same.
[Solution file)](exercise.files/comp.hs)

11. Looks familiar?

```hs
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    (<>) = undefined

main = do
  let failure :: String
              -> Validation String Int
  failure     = Failure
  success     :: Int
              -> Validation String Int
  success     = Success

print $ success 1 <> failure "blah"
print $ failure "woot" <> failure "blah"
print $ success 1 <> success 2
print $ failure "woot" <> success 2
```

You should get this output:
```
??> main
Success 1
Failure "wootblah"
Success 1
Success 2
```
[Solution file](exercise.files/validation.hs)


## Monoid exercises

Given a datatype, implement the `Monoid` instance. Add `Monoid` constraints to type variables where needed. For the datatypes you've already implemented `Semigroup` instances for, you need to figure out what the identity values is.

1. Again, validate _all_ of your instances with `QuickCheck`. Example scaffold is provided for the `Trivial` type.

```hs
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) = undefined

instance Monoid Trivial where
  mempty = undefined
  mappend = (<>)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)
```
[Solution file (can be run with cabal directly)](exercise.files/trivial.hs)

2. Validate `Monoid` properties using `QuickCheck`.

```hs
newtype Identity a =
  Identity a deriving Show
```
[Solution file (can be run with cabal)](exercise.files/identity.hs)

3. Validate `Monoid` properties using `QuickCheck`.

```hs
data Two a b = Two a b deriving Show
```
[Solution file (can be run with cabal)](exercise.files/two.hs)

4. Validate `Monoid` properties using `QuickCheck`.

```hs
newtype BoolConj =
  BoolConj Bool
```

What it should do:

```
??> (BoolConj True) `mappend` mempty
BoolConj True
??> mempty `mappend` (BoolConj False)
BoolConj
```
[Solution file (can be run with cabal)](exercise.files/boolConj.hs)

5. Validate `Monoid` properties using `QuickCheck`.

```hs
newtype BoolDisj =
  BoolDisj Bool
```
```
??> (BoolDisj True) `mappend` mempty
BoolDisj True
??> mempty `mappend` (BoolDisj)
BoolConj
```
[Solution file (can be run with cabal)](exercise.files/boolDisj.hs)


6. Validate `Monoid` properties using `QuickCheck`.

```hs
newtype Combine a b =
  Combine {unCombine :: (a -> b)}
```

What it should do:

```
??> f = Combine $ \n -> Sum (n + 1)
??> unCombine (mappend f mempty) $ 1
Sum {getSum = 2}
```
[Solution file](exercise.files/combine.hs)

7. Hint: We can do something that seems a little more specific and natural to function no that the input and output types are the same.

```hs
newtype Comp a =
  Comp (a -> a)
```
[Solution file](exercise.files/comp.hs)

8. This next exercise will involve doing something that will feel a bit unnatural still and you may find it difficult. We're going to toss you the instance declaration so you don't churn on a missing `Monoid` constraint you didn't know you needed.

```hs
newtype Mem s a =
Mem {
  runMem :: s -> (a, s)
}

instance Monoid a => Monoid (Mem s a) where
  mempty = undefined
  mappend = undefined
```

Given the following code:

```hs
f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
    rmleft = runMem (f' <> mempty) 0
    rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
```

A correct `Monoid` for `Mem` should, given the above code, get the following output:

```
??> main
("hi", 1)
("hi", 1)
("", 0)
True
True
```
Make certain your instance has output like the above. This is sanity checking the `Monoid` identity laws for you! It's not a proof and it's not even as good as property testing, but it'll catch the most common mistakes people make.

It's not a trick and you don't need a `Monoid` for `s`. Yes, such a `Monoid` can and does exist. Hint: chain the `s` values from one function to the other. You'll want to check the identity laws as a common first attemt will break them.
[Solution file](exercise.files/mem.hs)







