# Solutions to problems of chapter 8

## Recursion exercise

Write out the evaluation of the following. It might be a little less noisy if you do so wit the form that didn't use the composition operator `(.)`.

```hs
applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b =
    b
applyTimes n f b =
    f . applyTimes (n-1) f $ b
```

Solution:

```
applyTimes 5 (+1) 5

(+1) . applyTimes 4 (+1) $ 5
(+1) . (+1) . applyTimes 3 (+1) $ 5
(+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
(+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5
(+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
(+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) 5 -- This is where the base case hits
(+1) . (+1) . (+1) . (+1) . (+1) . 5
(+1) . (+1) . (+1) . (+1) . 6
(+1) . (+1) . (+1) . 7
(+1) . (+1) . 8
(+1) . 9
10
```

# Chapter Exercises

## Review of Types

1. What is the type of `[[True, False], [True, True], [False, True]]?

- [ ] a. `Bool`
- [ ] b. mostly `True`
- [ ] c. `[a]`
- [x] d. [[Bool]]

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`.

- [ ] a. `[(True, False), (True, True), (False, True)]`
- [x] b. `[3 == 3], [6 > 5], [3 < 4]]`
- [ ] c. `[3 == 3, 6 > 5, 3 < 4]`
- [ ] d. `["Bool", "more Bool", "Booly Bool!"]`

3. For the following function

```hs
func    :: [a] -> [a] -> [a]
func x y = x ++ y
```

which of the following is true?

- [x] a. `x` and `y` must be of the same type
- [x] b. `x` and `y` must both be lists
- [x] c. if `x` is a `String` then `y` must be a `String`
- [x] d. all of the above

4. For the `func` code above, which is valid application of `func` to both ot its arguments?

- [ ] a. `func "Hello World"`
- [x] b. `func "Hello" "World"`
- [ ] c. `func [1, 2, 3] "a, b, c"`
- [ ] d. `func ["Hello", "World"]`

## Reviewing currying

Given the following definitions, tell us what value results from further application.

```hs
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

--fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. What is the value of `appedCatty "woohoo!"? Try to determine the answer for yourself, then test in the REPL.

```
appedCatty "woohoo!" ->
cattyConny "woops" "woohoo!" ->
woops" ++ " mrow " "woohoo!" ->
"woops mrow woohoo!"
```

2. What is the value of `frappe "1"`?

```
frappe "1" ->
flippy "haha" "1" ->
flip cattyConny "haha" "1" ->
cattyConny "1" "haha" ->
"1" ++ " mrow " ++ "haha"
"1 mrow haha"
```

3. What is the value of `frappe (appedCatty "2")`

```
frappe (appedCatty "2") ->
frappe (cattyConny "woops" "2") ->
frappe ("woops" ++ " mrow " ++ "2") ->
frappe "woops mrow 2" ->
flippy "haha" "woops mrow 2" ->
flip cattyConny "haha" "woops mrow 2" ->
cattyConny "woops mrow 2" "haha" ->
"woops mrow 2" ++ " mrow " ++ "haha" ->
"woops mrow 2 mrow haha"
```

4. What is the value of `appedCatty (frappe "blue")`?

```
appedCatty (frappe "blue") ->
appedCatty (flippy "haha" "blue") ->
appedCatty (flip cattyConny "haha" "blue) ->
appedCatty (cattyConny "blue" "haha") ->
appedCatty ("blue" ++ " mrow " ++ "haha") ->
appedCatty "blue mrow haha" ->
cattyConny "woops" "blue mrow haha" ->
"woops" ++ " mrow " ++ "blue mrow haha" ->
"woops mrow blue mrow haha"
```

5. What is the value of

```
cattyConny (frappe "pink")
           (cattyConny "green" (appedCatty "blue"))
```

```
cattyConny (frappe "pink")
           (cattyConny "green" (appedCatty "blue")) ->
cattyConny (flippy "haha" "pink")
           (cattyConny "green" (cattyConny "woops" "blue")) ->
cattyConny (flip cattyConny "haha" "pink")
           (cattyConny "green" ("woops" ++ " mrow " ++ "blue")) ->
cattyConny (cattyConny "pink" "haha")
           (cattyConny "green" ("woops mrow blue")) ->
cattyConny ("pink" ++ " mrow " ++ "haha")
           ("green" ++ " mrow " ++ "woops mrow blue") ->
cattyConny "pink mrow haha" "green mrow woops mrow blue" ->
"pink mrow haha" ++ " mrow " ++ "green mrow woops mrow blue" ->
"pink mrow haha mrow green mrow woops mrow blue"
```

6. What is the value of `cattyConny (flippy "Pugs" "are") "awesome"`?

```
cattyConny (flippy "Pugs" "are") "awesome" ->
cattyConny (flip cattyConny "Pugs" "are") "awesome" ->
cattyConny (cattyConny "are" "Pugs") "awesome" ->
cattyConny ("are" ++ " mrow " ++ "Pugs") "awesome" ->
cattyConny "are mrow Pugs" "awesome" ->
"are mrow Pugs" ++ " mrow " ++ "awesome" ->
"are mrow Pugs mrow awesome"
```

## Recursion

1. Write out the steps for reducing `divideBy 15 2` to its final answer according to the Haskell code.

```hs
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

divideBy :: Numerator -> Denominator -> (Quotient, Remainder)
divideBy n d = go n d 0
  where go n   d    c
         | n < d = (c, n)
         | otherwise =
           go (n-d) d (c+1)
```

```
divideBy 15 2

go 15 2 0
go (15-2) 2 (0+1)
go (13-2) 2 (1+1)
go (11-2) 2 (2+1)
go (9-2) 2 (3+1)
go (7-2) 2 (4+1)
go (5-2) 2 (5+1)
go (3-2) 2 (6+1)
-- we hit the base case since 1 < 2
(7, 1)
```

2. Write a function that recursively sums all numbers from 1 to `n`, `n` being the argument. So that if n was 5, you'd add `1+2+3+4+5` to get `15`. The type should be `(Eq a, Num a) => a -> a`.

```hs
-- recursive approach with pattern matching
sumToN :: (Eq a, Num a) => a -> a
sumToN 1 == 1
sumToN n = n + sumToN (n-1)

-- recursive approach using guard
sumToN :: (Eq a, Num a) => a -> a
sumToN n
  |    n == 1 = 1
  | otherwise = n + sumToN (n-1)

-- iterative
sumToN :: (Num a) => a -> a
sumToN n = foldl (+) 0 [1..n]

-- math formula
sumToN :: (Num a) => a -> a
sumToN = n * (n-1) `div` 2
```
