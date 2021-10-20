# Solutions to problems of chapter 16

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What's the kind of _a_?

```hs
a -> a
```
**Answer:** The kind of `a` is `*`

2. What are the kinds of _b_ and _T_? (The _T_ is capitalized on purpose!)

```hs
a -> b a -> T (b a)
```

**Answer:** The kind of `b` is `* -> *` and the kind of `T` is also `* -> *`.

3. What's the kind of _c_?

```hs
c a b -> c b a
```

**Answer:** `c` is of kind `* -> * -> * `.


## Heavy Lifting

Add `fmap`, parentheses, and function composition to the expression as needed for the expression to typecheck and produce the expected result. It may not always need to go in the same place, so don't get complacent.

1. For the following statement:

```hs
a = (+1) $ read "[1]" :: [Int]
```
The expected result is:
```
λ> a
[2]
```

**Answer:**
```hs
a = fmap (+1) $ read "[1]" :: [Int]
```

2. For the following statement:

```hs
b = (++ "lol") (Just ["Hi,", "Hello"])
```

The expected result is:
```
λ> b
Just ["Hi,lol", "Hellolol"]
```

**Answer:**
```hs
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
```

3. For the following statement:

```hs
c = (*2) (\x -> x - 2)
```

The expected result is:
```
λ> c 1
-2
```

**Answer:**
```hs
c = fmap (*2) (\x -> x -2)
```

4. For the following statement:

```hs
d = ((return '1' ++) . show) (\x -> [x, 1..3])
```

The expected result is:
```
λ> d 0
"1[0,1,2,3]"
```

**Answer:**
```hs
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
```

5. For the following code snippet:

```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi
    in (*3) changed
```

The expected result is:
```
λ> e
3693
```

**Answer:**
```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
```

## Instances of Func

Implement `Functor` instances for the following datatypes. Use the `QuickCheck` properties we showed you to validate them.

1. For the following data type

```hs
newtype Identity a = Identity a
```
[Solution file](exercise.files/identityFunctor.hs)

2. For the following data type

```hs
data Pair a = Pair a a
```
[Solution file](exercise.files/pairFunctor.hs)

3. For the following data type

```hs
data Two a b = Two a b
```
[Solution file](exercise.files/twoFunctor.hs)

4. For the following data type

```hs
data Three a b c = Three a b c
```
[Solution file](exercise.files/threeFunctor.hs)

5. For the following data type

```hs
data Three' a b = Three' a b b
```
[Solution file](exercise.files/threePrimeFunctor.hs)


6. For the following data type

```hs
data Four a b c d = Four a b c d
```
[Solution file](exercise.files/fourFunctor.hs)

7. For the following data type

```hs
data Four' a b = Four' a a a b
```
[Solution file](exercise.files/fourPrimeFunctor.hs)


8. Can you implement a `Functor` instance for this type? Why? Why not?

```hs
data Trivial = Trivial
```
No, I can not. A `Functor` instance needs a type of kind `* -> *`  while `Trivial` has kind `*`.