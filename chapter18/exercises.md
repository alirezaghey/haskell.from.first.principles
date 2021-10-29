# Solutions to problems of chapter 18

## Implement `bind`

Write `bind` in terms of `fmap` and `join`.
```hs
bind :: Monad m => (a -> m b) -> m a -> m b
bind = undefined
```
**Answer:**
```hs
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x
```
[Solution file](exercise.files/bind.hs)

## Either Monad

Implement the `Either` Monad

```hs
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = undefind

instance Applicative (Sum a) where
  pure = undefined
  (<*>) = undefined

instance Monad (Sum a) where
  return = pure
  (>>=) = undefined
```
[Solution file](exercise.files/eitherMonad.hs)

# Chapter exercises

## implementing Monad instances

Write `Monad` instances for the following types. Use the `QuickCheck` properties we showed you to validate your instances.

1. For the `Nope` data type.

```hs
data Nope a = NopeDotJpg
```
[Solution file (can be run as a script)](exercise.files/nopeMonad.hs)

2. For the `PhhhbbtttEither` data type.

```hs
data PhhhbbtttEither b a =
    Left a
  | Right b
```
[Solution file (can be run as a script)](exercise.files/eitherMonad.hs)