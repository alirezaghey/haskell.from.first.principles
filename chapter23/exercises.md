# Solutions to problems of chapter 23

## Roll Your Own

1. Refactor `rollsToGetTwenty` into having the limit be a function argument.

```hs
rollsToGetN :: Int -> StdGen -> Int
rollsToGetn = undefined
```
**Answer:**
```hs
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n = count
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen
```
[Solution file](exercise.files/RandomExample2.hs)

2. Change `rollsToGetN` to recording the series of die that occured in addition to the count.

```hs
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged = undefined
```
**Answer:**
```hs
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 g [] where
  go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
  go sum count gen xs 
    | sum >= n = (count, xs)
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen (intToDie die:xs)
```
[Solution file](exercise.files/RandomExample2.hs)