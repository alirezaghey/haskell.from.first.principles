# Definitions

1. _Typeclass inheritance_ is when a typeclass has a superclass. This is a way of expressing that a typeclass requires _another_ typeclass to be available for a given type before you can write an inheritance.

```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
```

Here the typeclass Fractional _inherits_ from `Num`. We could also say that `Num` is a _superclass_ of `Fractional`. The long and short of it is that if you want to write an instance of `Fractional` for some `a`, that type `a`, must already have an instance of `Num` before you may do so.

```haskell
-- Even though in principle
-- this could work, it will fail because
-- Nada doesn't have a Num instance

newtype Nada =
  Nada Double deriving (Eq, Show)

instance Fractional Nada where
  (Nada x) / (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)

```

Then if you try to load it:

```
No instance for (Num Nada)
  arising from the superclasses
  of an instance declaration
In the instance declaration for
  `Fractional Nada`
```

You need a `Num` instance first. Can't write one that makes sense? Then you're not allowed to have a `Fractional` instance either. Them's the rules.

2. _Effects_ are how we refer to _observable_ actions programs may take other than compute a value. If a function modiefies some state or interacts with the outside world in a manner that can be observed, then we say it has an _effect_ on the world.
3. `IO` is the type for values whose evaluation bears the possibility of causing side effects, such as printing text, reading text input from the user, reading or writing files, or connecting to remote computers. This will be explained in _much_ more depth in the chapter on `IO`.
4. An _instance_ is the definition of how a typeclass should work for a given type. Instances are unique for a given combination of typeclass and type.
5. In Haskell we have _derived instances_ so that obvious or common typeclasses, such as `Eq`, `Enum`, `Ord`, and `Show` can have the instances generated based only on how the datatype is defined. This is so programmers can make use of these conveniences without writing the code themselves, over and over.
