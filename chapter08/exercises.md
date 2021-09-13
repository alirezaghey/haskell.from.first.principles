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
