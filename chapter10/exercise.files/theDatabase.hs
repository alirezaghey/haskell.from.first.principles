module TheDatabase where
import Data.Time


data DatabaseItem = DbString    String
                  | DbNumber    Integer
                  | DbDate      UTCTime 
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate $ UTCTime  (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123)
    , DbNumber 9001
    , DbString "Hello, World!"
    , DbDate $ UTCTime  (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123)
    ]
    


-- filters DbDate instances and returns their UTCTime as a list
filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldr f []
  where 
        f (DbDate a) b = a : b
        f  _         b = b
        

-- filters DbNumber instances and returns their Integer as a list
filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr f []
  where
        f (DbNumber a) b = a : b
        f _            b = b

-- gets the most recent date in the database
mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = foldr f z
  where
        f (DbDate a) b = if a > b then a else b
        f _          b = b
        z              = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
