module Person where

import System.IO



type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson  ::  Name
          ->  Age
          ->  Either PersonInvalid Person
mkPerson  name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | age <= 0              = Left AgeTooLow
  | otherwise             =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter the person's name: "
  name <- getLine
  putStr "Please enter the person's age: "
  age <- readLn
  let person = mkPerson name age
  case person of
    (Left person) -> putStrLn $ "error:" ++ show person
    (Right (Person name age)) -> putStrLn $ "Person name: " ++ name ++ "\n"
                                            ++ "Person age: " ++ show age