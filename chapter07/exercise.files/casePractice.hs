module CasePractice where

functionC :: Ord p => p -> p -> p
functionC x y =
    case x > y of
        True -> x
        False -> y


functionAdd2 :: Integral p => p -> p
functionAdd2 n =
    case even n of
        True -> n+2
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0
