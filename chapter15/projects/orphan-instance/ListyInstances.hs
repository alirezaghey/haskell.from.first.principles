module ListyInstances where

import Data.Monoid
import Listy

instance Semigroup (Listy a) where
  (Listy x) <> (Listy y)    = Listy $ x <> y
instance Monoid (Listy a) where
  mempty  = Listy []
  mappend = (<>)
