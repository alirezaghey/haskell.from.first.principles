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