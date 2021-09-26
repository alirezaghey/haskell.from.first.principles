# Definitions


1. A _fold_ is a higher-order function which, given a function to accumulate the results and a recursive data structure, returns the built up value. Usually a "start value" for the accumulation is provided along with a function that can combine the type of values in the data structure with the accumulation. The term fold is typically used with reference to collections of values referenced by a recursive datatype. For a generalization of "breaking down structure", see _catamorphism_.
2. A _catamorphism_ is a generalization of folds to arbitrary datatypes. Where a fold allows you to break down a list into an arbitrary datatype, a catamorphism is a means of breaking down the structure of any datatype. The `bool :: a -> a -> Bool -> a` function in `Data.Bool` is an example of a simple catamorphism for a simple, non-collection datatype. Similarly, `maybe :: b -> (a -> b) -> Maybe a -> b` is the catamorphism for `Maybe`. See if you can notice a pattern:

```hs
data Bool = False | True
bool :: a -> a -> Bool -> a

data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b

data Either a b = Left a | Right b
either :: (a -> b)
       -> (b -> c)
       -> Either a b
       -> c
```

3. A _tail call_ is the final result of a function. Some examples of tail calls in Haskell functions:

```hs
f x y z = h (subFunction x y z)
  where subFunction x y z = g x y z
  -- the ``tail call`` is
  -- h (subFunction x y z)
  -- or more precisely, h.
```

4. _Tail recursion_ is a function whose tail calls are recursive invocations of itself. This is distinguished from functions that call other functions in their tail call.

```hs
f x y z = h (subFunction x y z)
  where subFunction x y z = g x y z
```
The above is not tail recursive, since the _tail call_ is _h_, not itself.
```hs
f x y z = h (f (x-1) y z)
```
This is still not a tail recursive function. _f_ is invoked again but not in the tail call of _f_; it's an argument to the tail call, namely _h_.
```hs
f x y z = f (x - 1) y z
```
This is tail recursive. _f_ is calling itself directly with no intermediaries.
```hs
foldr f z []        = z
foldr f z (x:xs)    = f x (foldr f z xs)
```
Not tail recursive, since we give up control to the combining function _f_ before continuing through the list. `foldr`'s recursive calls will bounce between `foldr` and _f_.
```hs
foldl f z []        = z
foldl f z (x:xs)    = foldl f (f z x) xs
```
Tail recursive. `foldl` invokes itself recursively. The combining function is only an argument to the recursive fold.