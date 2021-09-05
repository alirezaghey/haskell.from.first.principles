# Solutions to problems of chapter 3

## Scope

1. These lines of code are from a REPL session. Is _y_ in scope for _z_?

```REPL
> x = 5
> y = 7
> z = x * y
```

_Yes_

2. These lines of code are from a REPL session. Is _h_ in scope for _g_?

```REPL
> f = 3
> g = 6 \* f + h
```

_No_
_h_ does not seem to be declared previously.

3. This code sample is from a source file. Is everything we need to execute _area_ in scope?

```haskell
area d = pi \* (r \* r)
r = d / 2
```

_No_
Since _d_ is local to the function _area_ it can be accessed in the expression following _r_.

4. This code is also from a source file. Now are _r_ and _d_ in scope for area?

```haskell
area d = pi \* (r \* r)
  where r = d / 2
```

_Yes_
Since _r_ is now defined in the local scope of _area_, it can access _d_ which is defined in its wrapping scope.

## Reading syntax

1. For the following lines of code, read the syntax carefully and decide if they are written correctly.
   a. `concat [[1,2,3], [4,5,6]]`
   _correct syntax. `concat` takes a list of lists and returns one list of type equal to the nested lists after concatenating them together._
   b. `++ [1, 2, 3] [4, 5, 6]`
   _wrong syntax. `++` is a infix operator. In order to use it prefix, it must be wrapped in parenthesis as follows:_
   `(++) [1, 2, 3] [4, 5, 6]`
   c. `(++) "hello" " world"`
   _correct syntax. `++` takes two parameters and wrapped in parenthesis can be used as a prefix operator._
   d. `["hello" ++ " world]`
   _wrong syntax. `world` is not correctly wrapped in double quotes._
   e. `4 !! "hello"`
   _wrong syntax. (!!) is of type `[a] -> Int -> a` which means its first parameter is a list and the second parameter is an integer. The correct expression would be:_
   `"hello" !! 4`
   f. `(!!) "hello" 4`
   _correct syntax. (!!) takes a list and an Integer and the parenthesis makes it prefix._
   g. `take "4 lovely"`
   _wrong syntax. `take` has the type `Int -> [a] -> [a]` but we are passing only a string, `[Char]`, to it. The correct syntax is a follows:_
   `take 4 "lovely"`
   h. `take 3 "awesome"`
   _correct syntax._

2. We have two sets: The first set is lines of code while the other set is results. Read the code and decide which result belongs to which set.
   a. `concat [[1 * 6], [2 * 6], [3 * 6]]` => `[6, 12, 18]`
   b. `"rain" ++ drop 2 "elbow"` => `"rainbow"`
   c. `10 * head [1, 2, 3]` => `10`
   d. `(take 3 "Julie") ++ (tail "yes")` => `Jules`
   e. `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]` => `[2, 3, 5, 6, 8, 9]`

## Building functions

1. Given the list-manipulation functions mentioned in this chapter, write functions that take the following inputs and return the expected outputs.
   a. Given "Curry is awesome", return "Curry is awesome!"
   `concat ["Curry is awesome", "!"]` or `"Curry is awesome" ++ "!"`
   b. Given "Curry is awesome!", return "y".
   `drop 4 (take 5 "Curry is awesome!")` or `"Curry is awesome!" !! 4`
   c. Given "Curry is awesome!", return "awesome!".
   `drop 9 "Curry is awesome!"`

2. Now, take each of the above and rewrite it in a source file as a general function that could take different string inputs as arguments but retain the same behavior.
   [Solution file](./exercise.files/buildingFunctions2.hs)
3. Write a function of type `String -> Char` which returns the third character in a `String`.
   [Solution file](./exercise.files/buildingFunctions3.hs)
4. Now change that function so the string operated on is always the same and the variable represents the number of the letter you want to return.
   [Solution file](./exercise.files/buildingFunctions4.hs)
