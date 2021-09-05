module Print3Broken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Yarrrrr"


-- uppon loading, stack ghci will throw the following error

-- λ> :l mid.chapter.exercises/print3broken.hs 
-- [1 of 1] Compiling Print3Broken     ( mid.chapter.exercises/print3broken.hs, interpreted )
-- 
-- mid.chapter.exercises/print3broken.hs:5:12: error:
--     Variable not in scope: greeting :: String
--   |
-- 5 |   putStrLn greeting
--   |            ^^^^^^^^
-- Failed, no modules loaded.

-- This means that on line 5 `putStrLn` cant' access `greeting` because it's out of scope.
-- The reason is that `printSecond` does not pass `greeting` as an argument to `putStrLn`.
-- While on line 10 when we call `printSecond` `greeting` is in scope, `putStrLn` in the actual body of `printSecond` won't have access to it unless `printSecond` passes it down.

-- The fix is in `print3brokenFixed.hs`
