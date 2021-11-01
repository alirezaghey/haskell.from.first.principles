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

3. Write a `Monad` instance for `Identity`.

```hs
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  (<*>) = undefined

instance Monad Identity where
  return = pure
  (>>=) = undefined
```
[Solution file (can be run as a script)](exercise.files/identityMonad.hs)

4. This one should be easier than the `Applicative` instance was. Remember to use the `Functor` that `Monad` requires, then see where things go.

```hs
data List a =
    Nil
  | Const a (List a)
```
[Solution file (can be run as a script)](exercise.files/listMonad.hs)

## Function implementation using `Monad` and `Functor` instances

Implement the following functions using the methods provided by `Monad` and `Functor`. Using stuff like identity and composition is fine, but it has to typecheck with types provided.

1. For the following function signature:

```hs
j :: Monad m => m (m a) -> m a
```
Expecting the following behavior:
```
λ> j [[1, 2], [], [3]]
[1,2,3]
λ> j (Just (Just 1))
Just 1
λ> j (Just Nothing)
Nothing
λ> j Nothing
Nothing
```
**Answer:**
```hs
j :: Monad m => m (m a) -> m a
j x = x >>= id
```
[Solution file](exercise.files/funcImplementations.hs)

2. For the following function signature:

```hs
l1 :: Monad m => (a -> b) -> m a -> m b
```
**Answer:**
```hs
{-
Different ways of implementing l1
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= (return . f)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x <&> f
-}
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
```
[Solution file](exercise.files/funcImplementations.hs)

3. For the following function signature:

```hs
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
```
**Answer:**
```hs
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2
```
[Solution file](exercise.files/funcImplementations.hs)

4. For the following function signature:

```hs
a :: Monad m => m a -> m (a -> b) -> m b
```
**Answer:**
```hs
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)
```
[Solution file](exercise.files/funcImplementations.hs)

5. You'll need recursion for this one.

```hs
meh :: Monad m => [a] -> (a -> m b) -> m [b]
```

**Answer:**
```hs
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= \b -> meh as f >>= \bs -> return $ b : bs
```
[Solution file](exercise.files/funcImplementations.hs)