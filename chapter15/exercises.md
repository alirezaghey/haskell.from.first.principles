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