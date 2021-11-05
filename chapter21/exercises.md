# Solutions to problems of chapter 20
# Chapter Exercises

## Traversable instances

Write a `Traversable` instance for the datatype provided, filling in any required superclasses. Use `QuickCheck` to validate your instances.

1. `Identity`

Write a `Traversable` instance for `Identity`.
```hs
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Traversable Identity where
  traverse = undefined
```
[Solution file (can be run as a script)](exercise.files/identityTraversable.hs)

2. `Constant`

```hs
newtype Constant a b = Constant {getConstant :: a}
```
[Solution file (can be run as a script)](exercise.files/constantTraversable.hs)

3. `Maybe`

```hs
data Optional a = Nada | Yep a
```
[Solution file (can be run as a script)](exercise.files/optionalTraversable.hs)

4. `List`

```hs
data List a = Nil | Cons a (List a)
```
[Solution file (can be run as a script)](exercise.files/listTraversal.hs)