module Cipher (caesar, unCaesar) where

import Data.Char
import System.IO


caesar :: IO ()
caesar = do 
  hSetBuffering stdout NoBuffering
  putStr "Please enter the string to be ciphered: "
  str <- getLine
  putStr "Please enter the number of shifts: "
  i <- readLn
  print $ caesar' str i
  
unCaesar :: IO ()
unCaesar = do 
  hSetBuffering stdout NoBuffering
  putStr "Please enter the string to be unciphered: "
  str <- getLine
  putStr "Please enter the number of unshifts: "
  i <- readLn
  print $ unCaesar' str i


caesar' :: [Char] -> Int -> [Char]
caesar' s i = map shiftChar s where
    shiftChar c = chr $ rem (ord c + i') 256 where
        i' = rem (abs i) 256

unCaesar' :: [Char] -> Int -> [Char]
unCaesar' s i = map shiftChar s where
    shiftChar c = chr $ if num < 0 then 256 + num else num where
        num = ord c - rem (abs i) 256 