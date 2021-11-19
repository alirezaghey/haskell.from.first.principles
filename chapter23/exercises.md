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


## Write `State` for yourself

1. State Functor
Write the `Functor` instance for the following:

```hs
newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = ???
```
**Answer:**
```hs
{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x -> let (y, z) = g x in
                                   (f y, z)
```
[Solution file](exercise.files/Moi.hs)

2. State Applicative

Write the `Applicative` instance for `State`.
```hs
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = ???

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moid f) <*> (Moi g) = ???
```
**Answer:**
```hs
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  -- pure a = Moi $ \s -> (a, s)
  -- Using TupleSection (it's the same as above)
  pure a = Moi (a, )
  
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (fun, ns) = f s
                                        (arg, fs) = g ns
                                    in  (fun arg, fs)
```
[Solution file](exercise.files/Moi.hs)