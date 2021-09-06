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
