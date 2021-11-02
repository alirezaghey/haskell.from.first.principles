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