module IfElseBool where
import Data.Bool

fIfElse :: Integral a => [a] -> [a]
fIfElse = map (\x -> if x == 3 then (-x) else x)

-- above function using bool
fBool :: Integral a => [a] -> [a]
fBool = map (\x -> bool x (-x) (x==3))