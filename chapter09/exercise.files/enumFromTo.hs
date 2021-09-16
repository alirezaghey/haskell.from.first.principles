module MyEnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool s e
  | s == e    = [s]
  | otherwise = [s, e]

eftOrd :: Ordering 
       -> Ordering 
       -> [Ordering]
eftOrd s e 
  | s == e              = [s]
  | s == LT && e == EQ  = [LT, EQ]
  | s == LT && e == GT  = [LT, EQ, GT]
  | s == EQ && e == GT  = [EQ, GT]
  | otherwise           = []

-- eftOrd using pattern matching
eftOrd2 :: Ordering
        -> Ordering
        -> [Ordering]
eftOrd2 LT EQ = [LT, EQ]
eftOrd2 LT GT = [LT, EQ, GT]
eftOrd2 EQ GT = [EQ, GT]
eftOrd2 s e
  | s == e    = [s]
  | otherwise = []

-- eftOrd using succ
eftOrd3 :: Ordering
        -> Ordering 
        -> [Ordering]
eftOrd3 s e
  | s == e    = [s]
  | otherwise = s : eftOrd3 (succ s) e


eftInt :: Int -> Int -> [Int]
eftInt s e
  | s == e    = [s]
  | s < e     = s : eftInt (s+1) e
  | otherwise = []

-- eftInt using succ
eftInt2 :: Int -> Int -> [Int]
eftInt2 s e
  | s == e    = [s]
  | s < e     = s : eftInt2 (succ s) e
  | otherwise = []


eftChar :: Char -> Char -> [Char]
eftChar s e
  | s == e      = [s]
  | s < e       = s : eftChar (succ s) e
  | otherwise   = []