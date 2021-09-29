module Phone where

type Digit    = Char
type Presses  = Int

data DaPhone  = DaPhone [Button]

data Button   = Button Digit [Char]

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
                , Button '0' "0+_"
                , Button '#' "#.,"
                ] 