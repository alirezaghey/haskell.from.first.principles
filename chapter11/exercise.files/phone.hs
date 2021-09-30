module Phone where

import Data.Char (isUpper, toUpper, toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (elemIndex, maximumBy)


type Digit    = Char
type Presses  = Int

newtype DaPhone  = DaPhone [Button]

data Button   = Button Digit [Char]

-- represents an old school phone keyboard
phone :: DaPhone
phone = DaPhone [ Button '1' "1"
                , Button '2' "2abc"
                , Button '3' "3def"
                , Button '4' "4ghi"
                , Button '5' "5jkl"
                , Button '6' "6mno"
                , Button '7' "7pqrs"
                , Button '8' "8tuv"
                , Button '9' "9wxyz"
                , Button '*' "*^"
                , Button '0' "0+_ "
                , Button '#' "#.,"
                ] 

-- calculates which button how many times must be pressed
-- to produce a specific character
reverseTaps :: DaPhone
            -> Char 
            -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) c
  | isUpper c = ('*', 1) : [charToTap buttons $ toLower c]
  | otherwise = [charToTap buttons c]
  where
      charToTap ((Button d s):bs) c
        | c == '1'        = ('1', 1)
        | isNothing num   = charToTap bs c
        | otherwise       = (d, if fromJust num == 0 then length s else fromJust num)
        where
          num = elemIndex c s
      charToTap [] _                = error ("where phone? or what char is" ++ [c] ++ "?")


-- produces the sequence of digits in the form of (digit, #press)
-- that must be pressed to produce a specific message
cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps


convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
  
-- calculates how many key presses are
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd


-- returns the letter that's most repeated in a string
-- ignores case
mostPopularLetter :: String -> Char 
mostPopularLetter text = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ freqLetter text where
  freqLetter :: String -> [(Char, Int)]
  freqLetter text = [(c, length $ filter (\x -> toLower x==c) text) | c <- ['a'..'z']]

-- calculates the cost (num of required presses)
-- of the most frequent char in a string
costOfMostPopularChar :: String -> Presses
costOfMostPopularChar = fingerTaps . reverseTaps phone . mostPopularLetter