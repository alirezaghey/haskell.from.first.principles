module EqInstanceExer where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int)
       (TisAn int') =
     int == int'