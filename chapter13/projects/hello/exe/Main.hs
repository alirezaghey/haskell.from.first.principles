module Main where

import Hello
import DogsRule

main :: IO ()
main = do
  name <- getLine
  sayHello name
  dogs
