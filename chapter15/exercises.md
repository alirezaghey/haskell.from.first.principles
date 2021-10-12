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
