data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f ini (Constant x) = f x ini