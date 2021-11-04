# Solutions to problems of chapter 20

## Library Functions

Implement the functions in terms of `foldMap` or `foldr` from `Foldable`, then try them out with multiple types that have `Foldable` instances.

1. The following function:

```hs
sum :: (Foldable t, Num a) => t a -> a
```
**Answer:**
```hs
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0
```
[Solution file](exercise.files/libraryFunctions.hs)

2. The following function:

```hs
product :: (Foldable t, Num a) => t a -> a
```
**Answer:**
```hs
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1
```
[Solution file](exercise.files/libraryFunctions.hs)

3. The following function:

```hs
elem :: (Foldable t, Eq a) => a -> t a -> Bool
```
**Answer:**
```hs
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y acc -> y == x) False
```
[Solution file](exercise.files/libraryFunctions.hs)


4. The following function:

```hs
minimum :: (Foldable t, Ord a) => t a -> Maybe a
```
**Answer:**
```hs
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x y -> min <$> Just x <*> y) Nothing
```
[Solution file](exercise.files/libraryFunctions.hs)

5. The following function:

```hs
maximum :: (Foldable t, Ord a) => t a -> Maybe a
```
**Answer:**
```hs
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x y -> min <$> Just x <*> y) Nothing
```
[Solution file](exercise.files/libraryFunctions.hs)

6. The following function:

```hs
null :: (Foldablee t) => t a -> Bool
```
**Answer:**
```hs
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True
```
[Solution file](exercise.files/libraryFunctions.hs)

7. The following function:

```hs
length :: (Foldable t) => t a -> Int
```
**Answer:**
```hs
length :: (Foldable t) => t a -> Int
length = foldr (\_ x -> x+1) 0
```
[Solution file](exercise.files/libraryFunctions.hs)

8. The following function:

```hs
toList :: (Foldable t) => t a -> [a]
```

**Answer:**
```hs
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []
```
[Solution file](exercise.files/libraryFunctions.hs)

9. The following function:

```hs
-- Combine the elements
-- of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
```
**Answer:**
```hs
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id
```
[Solution file](exercise.files/libraryFunctions.hs)

10. Define `foldMap` in terms of `foldr`.

```hs
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```
**Answer:**
```hs
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty
```
[Solution file](exercise.files/libraryFunctions.hs)

# Chapter Exercises

Write `Foldable` instances for the following datatypes.

1. For the following data type:

```hs
data Constant a b = Constant b
```
**Answer:**
```hs
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f ini (Constant x) = f x ini
```
[Solution file](exercise.files/foldableInstances.hs)

2. For the following data type:

```hs
data Two a b = Two a b
```
**Answer:**
```hs
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f ini (Two _ x) = f x ini
```
[Solution file](exercise.files/foldableInstances.hs)


3. For the following data type:

```hs
data Three a b c = Three a b c
```

**Answer:**
```hs
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f ini (Three _ _ x) = f x ini
```
[Solution file](exercise.files/foldableInstances.hs)

4. For the following data type:

```hs
data Three' a b = Three' a b b
```

**Answer:**
```hs
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f ini (Three' _ x y) = f x $ f y ini
```
[Solution file](exercise.files/foldableInstances.hs)

5. For the following data type:

```hs
data Four' a b = Four' a b b b
```

**Answer:**
```hs
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f ini (Four' _ x y z) = f x $ f y $ f z ini
```
[Solution file](exercise.files/foldableInstances.hs)

Write a filter function for `Foldable` types using `foldMap`.
```hs
filterF :: (Applicative f
          , Foldable t
          , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF = undefined
```

**Answer:**
```hs
filterF :: (Applicative f
          , Foldable t
          , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
```
[Solution file](exercise.files/foldableInstances.hs)