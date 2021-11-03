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