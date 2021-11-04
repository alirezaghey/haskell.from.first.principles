data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f ini (Constant x) = f x ini
  
----------------------------------------------
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f ini (Two _ x) = f x ini
  

---------------------------------------------
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f ini (Three _ _ x) = f x ini


--------------------------------------------
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f ini (Three' _ x y) = f x $ f y ini
  

--------------------------------------------
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f ini (Four' _ x y z) = f x $ f y $ f z ini