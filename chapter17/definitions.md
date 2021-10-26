# Definitions

1. `Applicative` can be thought of as characterizing monoidal functors in Haskell. For a Haskeller's purpose, it's a way to functionally apply a function which is embedded in structure _f_ of the same type as the value your're mapping it over.

```hs
fmap  ::    (a -> b) -> f a -> f b
(<*>) :: f  (a -> b) -> f a -> f b
```