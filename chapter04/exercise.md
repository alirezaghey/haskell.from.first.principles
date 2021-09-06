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
