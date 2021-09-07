# Solutions to problems of chapter 4

## Mood Swing

Given the following datatype, answer the following questions:

```haskell
data Mood = Blah | Woot deriving Show
```

1. What is the type constructor, or name of this type?
   <br>**Answer:** Mood
2. If the function requires a `Mood` value, what are the values you could possibly use?
   <br>**Answer:** `Blah` and `Woot`
3. We are trying to write a function `changeMood` to change Chris's mood instantaneously. It should act like `not` in that, given one value, it returns the other value of the same type. So far, we've written a type signature `changeMood :: Mood -> Woot`. What's wrong with that?
   <br>**Answer:** The return type is a value constructor and not a type constructor. The correct type signature is `changeMood :: Mood -> Mood`.
4. Now we want to write the function that changes his mood. Given an input `Mood`, it should return the opposite `Mood`. Fix any mistakes and complete the function:

```REPL
>:{
> changeMood :: Mood -> Mood
> changeMood Blah = Woot
> changeMood    _ = Blah
>:}
```

Wrapping the lines in `:{` and `:}` allows you to write multi-line code in the REPL.

5. Enter all of the above -- datatype (including the `deriving Show` bit), your corrected type signature, and the corrected function into a source file. Load it and run it in GHCi to make sure you got it right.
   <br> [Solution file](./exercise.files/moodswing.hs)

## Find the Mistakes

The following liens of code may have mistakes. Fix them!

1. `not True && true`
   <br> **Answer:** `not True && True`
   <br>_Note:_ Keep in mind that while the results would be the same, this expression is not exactly the same as `not (True && True)`. Named functions have higher precedence than operators. Thus, `not True && False` is `False` while `not (True && False)` is `True`.
2. `not (x = 6)`
   <br> **Answer:** `not (x == 6)`, provided that `x` is in scope.
3. `(1 \* 2) > 5`
   <br> **Answer:** This is correct
4. `[Merry] > [Happy]`
   <br> **Answer:** `["Merry"] > ["Happy"]` unless `Merry` and `Happy` are variables in scope.
5. `[1, 2, 3] ++ "look at me!"`
   <br> **Answer:** `"1, 2, 3 " ++ "look at me!"`
   <br> _Note:_ The infix operator `++` has the type `[a] -> [a] -> [a]`. This means that the elements of the two lists must be of the same type.

# Chapter Exercises

## Unnamed Section

**For the following exercises, you need to have the following 3 variables in scope:**

```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```

`length` is a function that takes a list and returns a result that tells how many items are in the list:

1. Given the definition of `length` above, what is the type signature?
   <br>**Answer:** `length :: [a] -> Integer`
2. What are the results of the following expressions?
   <br>a. `length [1, 2, 3, 4, 5]`
   <br> **Answer:** `5`
   <br>b. `length [(1, 2), (2, 3), (3, 4)]`
   <br> **Answer:** `3`
   <br>c. `length allAwesome`
   <br> **Answer:** `2`
   <br>d. `length (coancat allAwesome)`
   <br> **Answer:** `5`
3. Given what we know about numeric types and the type signature of `length`, look at these two expressions. One works and one returns an error. Determine which will return an error and why.

```REPL
Presule> 6 / 3
-- and
Predule> 6 / length [1, 2, 3]
```

**Answer:** `6 / length [1, 2, 3]` returns an error. Reason is that `(/) :: Fractional a => a -> a -> a` means that both arguments to `/` must be `Fractional` values, but `length` returns an `Int` value. We could demonstrate that by forcing a number value to be an `Int`: `1 / 2 :: Int`, throws the same error.

4. How can you fix the broken code from the preceding exercise using a different division function/operator?
   <br> **Answer:** `div 6 $ length [1, 2, 3]`

5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?
   <br> **Answer:** `Bool` is the type and we can expect it to evaluate to `True`.
6. What is the type of the expected value of the following?

```REPL
Prelude> x = 5
Prelude> x + 3 == 5
```

**Answer:** `Bool` is the type and we can expect it to evaluate to `False`.

7. Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?

`Prelude> length allAwesome == 2`
<br> **Answer:** This will work and evaluate to `True`. Since `length` has precedence over `==` the expression is equivalent to: `length [awesome, also] $ == 2 -> 2 == 2 -> True`.

`Prelude> length [1, 'a', 3, 'b']`
<br> **Answer:** This won't work. Lists must contain elements of the same type. There are `Char`s and `Int`s in the list.

`Prelude> length allAwesome + length awesome`
<br> **Answer:** This will work and evaluate to `5`. Since `length` has precedence over `+` the expression is equivalent to: `(length allAwesome) + (length awesome) -> (length [awesome, also]) + (length ["Papuchon", "curry", ":)"]) -> 2 + 3 -> 5`.

`Prelude> (8 == 8) && ('b' < 'a')`
<br> **Answer:** This will work and evaluate to `False`. `(True) && (False) -> False`

`Prelude> (8 == 8) && 9`
<br> **Answer:** This won't work. The `&&` operator has type `Bool -> Bool -> Bool` while the right hand side is an `Int`.

8.  Write a function that tells you whether or not a given String is a palindrome. Here you'll want to use a function called `reverse`. A built-in function that does what it sounds like.

```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
```

9. Write a function to return the absolute value of a number using if-then-else.

```haskell
myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x
```

10. Fill in the definition of the following function, using `fst` and `snd`:

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

## Correcting syntax

In the following examples, you'll be shown syntactically incorrect code. Type it in and try to correct it in your text editor, validating it with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and returns that result.

```haskell
x = (+)
F xs =  w 'x' 1
  where w = length xs
```

Problems with this code:

- `x` represents the prefix add operator `(+)`. It can't be put into single quotes which makes it a `Char` and also must precede both its arguments. Alternatively, we can put `x` in backticks
  \``x`\` instead of single quotes which renders the prefix operator to an infix.
- `F` is supposed to be a function. Functions in Haskell can't start with a capital letter.

Correct code:

```haskell
x = (+)
f xs = x w 1
  where w = length xs
-- or
f xs = w `x` 1
  where w = length xs
```

2. This is supposed to be the identity function, `id`.

```haskell
\X = x
```

Problems with this code:

- Either this is supposed to be a lambda function, in wich case we have to use an `->` instead of `=` and also make the function parameter lower case
- Or it is supposed to be a named function like `id`. In that case we don't use `\`, the function must have a name and `X` must be lower case.

```haskell
\x -> x
-- or
myId :: a -> a
myId x = x
```

3. When fixed, this function will return 1 from the value (1, 2)

```haskell
f (a b) = A
```

There are two approaches to solve this:

- Using the built-in `fst` function
- Using pattern matching

```haskell
f :: (a, b) -> a
f x = fst x
--or
f :: (a, b) -> a
f (a, b) = a
```

## Math the function names to their types

1. Which of the following types is the type of `show`?
   <br>a. `show a => a -> String`
   <br>b. `Show a -> a -> String`
   <br>c. `Show a => a -> String`
   <br> **Answer:** c

2. Which of the following types is the type of `(==)`?
   <br>a. `a -> a -> Bool`
   <br>b. `Eq a => a -> a -> Bool`
   <br>c. `Eq a -> a -> a -> Bool`
   <br>d. `Eq a => A -> Bool`
   <br> **Answer:** b

3. Which of the following types is the type of `fst`?
   <br>a. `(a, b) -> a`
   <br>b. `b-> a`
   <br>c. `(a, b) -> b`
   <br> **Answer:** a

4. Which of the following types is the type of `(+)`?
   <br>a. `(+) :: Num a -> a -> a -> Bool`
   <br>b. `(+) :: Num a => a -> a -> Bool`
   <br>c. `(+) :: num a => a -> a -> a`
   <br>d. `(+) :: Num a => a -> a -> a`
   <br>f. `(+) :: a -> a -> a`
   <br> **Answer:** d

## Parametricity

All you can do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.

1. Given the type `a -> a`, which is the type for `id`, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.
