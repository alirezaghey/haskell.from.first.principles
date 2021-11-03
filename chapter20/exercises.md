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