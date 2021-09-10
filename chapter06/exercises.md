# Solutions to problems of chapter 6

## Eq Instances

Write the `Eq` instance for the datatype provided.

_Note:_ All the solutions to this exercise are in [exercise.files/EqInstancesExer.hs](exercise.files/EqInstancesExer.hs). In addition to the `prefix` implementation shown here, the file contains `infix` and `deriving` solutions too.

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

2.

```haskell
data TwoIntegers =
    Two Integer Integer
```

**Answer:**

```haskell
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2)
       (Two int1' int2') =
    int1 == int1' && int2 == int2'
```

3.

```haskell
data StringOrInt =
    TisAnInt Int
  | TisAString String
```

**Answer:**

```haskell
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int)
       (TisAnInt int') =
         int == int'
  (==) (TisAString str)
       (TisAString str') =
         str == str'
  (==) _ _ = False
```

4.

```haskell
data Pair a =
  Pair a a
```

**Answer:**

```haskell
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2)
       (Pair a1' a2') =
         a1 == a1' && a2 == a2'
```

5.

```haskell
data Tuple a b =
  Tuple a b
```

**Answer:**

```haskell
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b)
       (Tuple a' b') =
         a == a' && b == b'
```

6.

```haskell
data Which a =
    ThisOne a
  | ThatOne a
```

**Answer:**

```haskell
data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') =
    a == a'
  (==) (ThatOne a) (ThatOne a') =
    a == a'
  (==) _ _ = False
```

7.

```haskell
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a)
       (Hello a') =
         a == a'
  (==) (Goodbye b)
       (Goodbye b') =
         b == b'
  (==) _ _ = False
```

## Tuple experiment

Look at the types given for `quotRem` and `divMod`. What do you think those functions do? Test your hypotheses by playing with them in the REPL. We've given you a sample to start with below:

`ones x = snd (divMod x 10)`

**Answer:** `ones` takes x and integer divides it by 10. `divMod` returns a tuple where the first element is the quotient and the second element is the remainder. The `snd` function returns the second element of the tuple which is the remainer.

## Will The Work?

Take a look at teh following code examples and try to decide if they will work, what result they will return if they do, and why or why not (be sure, as always, to test them in your REPL once you have decided on your answer):

1.

```haskell
max (length [1, 2, 3])
    (length [8, 9, 10, 11, 12])
```

**Answer:** Yes, this will work and the result will be `5`. The signature for `max` is `max :: (Ord a) => a -> a -> a` and the signature of `length` is `length :: [a] -> Int`. This will render the concrete signature of `max` into `max :: Int -> Int -> Int`. Evaluating the expression give `max 3 5 = 5`.

2.

```haskell
compare (3 * 4) (3 * 5)
```

**Answer:** Yes, this will work and the result will be `LT`. The signature for `compare` is `compare :: (Ord a) => a -> a -> Ordering`. This means that `compare` takes 2 arguments of any type that has an instance for `Ord` and returns a value of type `Ordering`. The `Ordering` type has 3 constructors `LT`, `GT`, and `EQ`. The input arguments will render the concrete signature of `compare` to `compare :: Int -> Int -> Ordering`

3.

```haskell
compare "Julie" True
```

**Answer:** No, this will not work. `compare` has the type `compare :: (Ord a) => a -> a -> Ordering`. Notice that both parameters of `compare` _must_ have the same type. Even when both `[Char]` and `Bool` have an instance of `Ord` there is no way the compiler knows how to compare them with one another.

4.

```haskell
(5 + 3) > (3 + 6)
```

**Answer:** Yes, this will work and the result is `False`. The signature for `(>)` is `(>) :: (Ord a) => a -> a -> Bool`.

# Chapter Exercises

## Multiple choice

1. The `Eq` class

- [ ] a. includes all types in Haskell
- [ ] b. is the same as the `Ord` class
- [x] c. makes equality tests possible
- [ ] d. only includes numeric types

2. The typeclass `Ord`

- [x] a. allows any two values to be compared
- [ ] b. is a subclass of `Eq`
- [ ] c. is a superclass of `Eq`
- [ ] d. has no instancve for `Bool`

3. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?

- [x] `Ord a => a -> a -> Bool`
- [ ] `Ord a => Int -> Bool`
- [ ] `Ord a => a -> Char`
- [ ] `Ord a => Char -> [Char]

4. In `x = divMod 16 12`

- [ ] the type of `x` is `Integer`
- [ ] the value of `x` is undecidable
- [x] the type of `x` is a tuple.
- [ ] `x` is equal to `12 / 16`

_Note:_ `x` is of type `Integral a => (a, a)` to be exact.

5. The typeclass `Integral` includes

- [x] `Int` and `Integer` numbers
- [ ] integral, real, and fractional numbers
- [ ] Schrodinger's cat
- [ ] only positive numbers

## Does it typecheck?

For this section of exercises, you'll be practicing looking for type and typeclass errors.

For example, `printIt` will not work because functions like `x` have no instance of `Show`, the typeclass that lets you convert things to Strings (usually for printing):

```haskell
x    :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show x)
```

Here's the type error you get if you try to load the code:

```
No instance for (Show (Int -> Int)) arising
  from a use of `show`

In the first argument of `putStrLn`, namely `(show x)`
In the expression: putStrLn (show x)
In an equation for `printIt`: printIt = putStrLn (show x)
```

It's saying it can't find an implementation of the typeclass `Show` for the type `Int -> Int`, which makes sense. Nothing with the function type constructor `(->)` has an instance of `Show` by default in Haskell.

Examine the following code and decide whether it will typecheck. The load it in GHCi and see if you were correct. If it doesn't typecheck, try to match the type error against your understanding of why it didn't work. If you can, fix the error and re-run the code.

1. Does the following code typecheck? If not, why not?

```haskell
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```

**Answer:** No, it won't typecheck. The problem is that `Person` has not provided an instance for the typeclass `Show` that is required to create a `String` representation of a data type. Fix:

````haskell
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```

2. Does the following typecheck? If not, why not?

```haskell
data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                  then Blah
                  else x
````

**Answer:** It won't typecheck. `Mood` needs to implement the `Eq` typeclass for it to use `==`. Fix:

```haskell
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
                  else x
```

_Note:_ The compiler infers the type of `settlDown` from context and gives it `settleDown :: Mood -> Mood`
