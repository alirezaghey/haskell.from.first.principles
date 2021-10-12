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