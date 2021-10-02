-- As natural as any
-- competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

-- example input and output of natToInteger
-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Suc (Succ Zero))
-- 2
-- takes a Nat and returns its Integer value
natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n


-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >> IntegerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0       = Nothing
  | otherwise   = Just (go n) where
    go n
      | n == 0        = Zero
      | otherwise     = Succ (go (n-1))
