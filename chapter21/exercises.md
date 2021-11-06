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

5. `Three`

```hs
data Three a b c = Three a b c
```
[Solution file (can be run as a script)](exercise.files/threeTraversal.hs)

6. `Pair`

```hs
data Pair a b = Pair a b
```
[Solution file (can be run as a script)](exercise.files/pairTraversable.hs)

7. `Big`

When you have more than one value of type _b_, you'll want to use `Monoid` and `Applicative` for the `Foldable` and `Traversable` instances respectively.

```hs
data Big a b = Big a b b
```
[Solution file (can be run as a script)](exercise.files/bigTraversal.hs)

8. `Bigger`

```hs
data Bigger a b = Bigger a b b b
```
[Solution file (can be run as a script)](exercise.files/biggerTraversable.hs)

9. `S`

```hs
{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n
        , Arbitrary (n a)
        , Arbitrary a )
      => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n
        , Testable (n Property)
        , EqProp a)
      =>  EqProp (S n a) where
  (S x y) =-= (S p q) =
      (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

instance Traversable n => Traversable (S n) where
  traverse = undefined

main = sampl' (arbitrary :: Gen (S [] Int))
```
[Solution file (can be run as a standalone script)](exercise.files/skiFreeTraversable.hs)


## Instances for `Tree`

Write the following instances for `Tree`.

```hs
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap = undefined


-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap = undefined

instance Traversable Tree where
  traverse = undefined
```

**Hints:**
1. For `foldMap`, think `Functor` but with some `Monoid` thrown in.
2. For `traverse`, think `Functor` but with some `Functor` thrown in.
[Solution file (can be run as a standalone script)](exercise.files/treeTraversable.hs)
