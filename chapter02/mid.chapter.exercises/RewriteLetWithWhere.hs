module RewriteLetWithWhere where

-- rewrite the following let expressions with where

-- let x = 5 in x
func1    = x
 where x = 5

-- let x = 5 in x * x
func2    = x * x
 where x = 5

-- let x = 5; y = 6 in x * y
func3    = x * y
 where x = 5
       y = 6

-- let x = 3; y = 1000 in x * 3 + y
func4    =  x * 3 + y
 where x = 3
       y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
func5    = x * 5
 where x = 10 * 5 + y
       y = 10

{-|
  let x = 7
      y = negate x
      z = y * 10
  in z / x + y
-}
func6    = z / x + y
 where x = 7
       y = negate x
       z = y * 10 

