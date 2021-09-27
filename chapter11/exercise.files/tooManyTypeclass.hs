{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool 

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n
  

instance TooMany (Int, Int) where
  tooMany (n , m) = tooMany (n + m)

