# Solutions to problems of chapter 5

## Type Matching

Below you'll find a list of several standard functions we've talked about previously. Under that is a list of their type signatures. Mathch the function to its type signature.

1. Functions:
   <br>a. `not`
   <br>b. `length`
   <br>c. `concat`
   <br>d. `head`
   <br>e. `(<)`
2. Type signatures:
   <br>a. `_ :: [a] -> a`
   <br>b. `_ :: [[a]] -> [a]`
   <br>c. `_ :: Bool -> Bool`
   <br>d. `_ :: [a] -> Int`
   <br>e. `_ :: Ord a => a -> a -> Bool`

**Answers:** (a, c), (b, d), (c, b), (d, a), (e, e)

## Manual currying and un-currying

Load [curry_uncurry_anon.hs](./exercise.files/curry_uncurry_anon.hs) into GHCi and check that all the functions return the same result given equal input.

```REPL
Prelude> curriedFunction 10 True
815
Prelude> curriedFunction 20 False
31357
Prelude> uncurriedFunction (10, True)
815
Prelude> uncurriedFunction (20, False)
3157
Prelude> anonymous 10 True
815
Prelude> anonymous 20 False
3157
Prelude> anonNested 10 True
815
Prelude> anonNested 20 False
3157
```
