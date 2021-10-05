module Main where

import Hello
import DogsRule
import System.IO


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine
  sayHello name
  putStr "Please input your dog's name: "
  dogName <- getLine
  dogs dogName
