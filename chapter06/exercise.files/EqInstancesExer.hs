module EqInstanceExer where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int)
       (TisAn int') =
     int == int'

-- alternative infix implementation
-- instance Eq TisAnInteger where
--   (TisAn int) == (TisAn int') =
--     int == int'

-- alternatively you can just derive it from Eq
-- data TisAnInteger =
--  TisAn Integer deriving (Eq)

------------------------------------------------------------------

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2)
       (Two int1' int2') =
    int1 == int1' && int2 == int2'

-- alternative infix implementation
-- instance Eq TwoIntegers where
--   (Two int1 int2) == (Two int1' int2') =
--     int1 == int1' && int2 == int2'

-- alternatively you can just derive it from Eq
-- data TwoIntegers =
--   Two Integer Integer deriving (Eq)

-------------------------------------------------------------------

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int)
       (TisAnInt int') =
         int == int'
  (==) (TisAString str)
       (TisAString str') =
         str == str'
  (==) _ _ = False

-- alternative infix implementation
-- instance Eq StringOrInt where
--   (TisAnInt int) == (TisAnInt int') =
--     int == int'
--   (TisAString str) == (TisAString str') =
--     str == str'
--   _ == _ = False

-- alternatively you can just derive it from Eq
-- data StringOrInt =
--     TisAnInt Int
--   | TisAString String deriving (Eq)

------------------------------------------------------------------

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2)
       (Pair a1' a2') =
         a1 == a1' && a2 == a2'

-- alternative infix implementation
-- instance Eq a => Eq (Pair a) where
--   (Pair a1 a2) == (Pair a1' a2') =
--     a1 == a1' && a2 == a2'


-- alternatively you can just derive it from Eq
-- data Pair a =
--   Pair a a deriving (Eq)

------------------------------------------------------------------
