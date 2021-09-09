# Solutions to problems of chapter 6

## Eq Instances

Write the `Eq` instance for the datatype provided.

_Note:_ All the solutions to this exercise are in [exercise.files/EqInstancesExer.hs](exercise.files/EqInstancesExer.hs)

1. It's not a type, we're just being cute with the name.

```haskell
data TisAnInteger =
  TisAn Integer
```

**Answer:**

```haskell
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int)
       (TisAn int') =
     int == int'
```
