data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f ini (Constant x) = f x ini
  
----------------------------------------------
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f ini (Two _ x) = f x ini