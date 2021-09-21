module Cipher where

import Data.Char

caesar :: [Char] -> Int -> [Char]
caesar s i = map shiftChar s where
    shiftChar c = chr $ rem (ord c + i') 256 where
        i' = rem (abs i) 256

unCaesar :: [Char] -> Int -> [Char]
unCaesar s i = map shiftChar s where
    shiftChar c = chr $ if num < 0 then 256 + num else num where
        num = ord c - rem (abs i) 256 