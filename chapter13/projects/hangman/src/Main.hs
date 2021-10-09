module Main where

import Control.Monad        (forever)
import Data.Char            (toLower)
import Data.Maybe           (isJust, fromMaybe)
import Data.List            (intersperse)
import System.Exit          (exitSuccess)
import System.Random        (randomRIO)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle



newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in      l >= minWordLength
              &&  l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' '  (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (replicate (length w) Nothing) []

charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s) where
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar

    zipper guessed wordChar guessChar
      | wordChar == guessed = Just wordChar
      | otherwise  = guessChar
      
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn  $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess,
        alreadyGuessed puzzle guess) of

    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle

    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)
    
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if length (filter (\c -> not $ elem c wordToGuess) guessed) > 7 then
    do  putStrLn "You Lose!"
        putStrLn $ "The words was: " ++ wordToGuess
        exitSuccess
  else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _)
  | all isJust filledInSoFar = do
    putStrLn "You win!"
    exitSuccess
  | otherwise = do
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
