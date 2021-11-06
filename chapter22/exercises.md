# Solutions to problems of chapter 21

## FAM for functions

The `Functor` instance for functions is function composition. Meaning:
```
f <$> g == fmap f g == f . g
```

The `Applicative` and `Monad` instances for functions passes incoming arguments to _all_ the involved functions in the `Applicative`/`Monad` chain. Meaning:
```
(+) <$> (*2) <*> (+10) == liftA2 (+) (*2) (+10)
-- above is a function that takes one Num and returns one Num.
-- incoming Num is passed to both (*2) and (+10)
-- the results of those functions is passed to (+)
```
For `Monad`s:
```
test = do
  a <- (+1)
  b <- (*10)
  return (a + b)
-- above is a function that takes one Num and returns one Num.
-- incoming Num is passed to both (+1) and (*10)
-- the results of those functions is passed to (+)
```
These examples are in [this file](exercise.files/famForFunctions.hs)


## Warming Up

Implement the undefined functions. `composed` and `fmapped` should have identical results.
```hs
import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = undefined

fmapped :: [Char] -> [Char]
fmapped = undefined
```
[Solution file](exercise.files/famForFuncsExercise.hs)

Now we want to return the results of `cap` and `rev` both as a tuple like the following:
```
λ> tupled "Julie"
("JULIE", "eiluJ")
-- or
λ> tupled' "Julie"
("eiluJ", "JULIE")
```
The signature is as follows:
```hs
tupled :: [Char] -> ([Char], [Char])
```
Write both a definition using the `Applicative` and `Monad` instances for functions.
[Solution file](exercise.files/famForFuncsExercise.hs)