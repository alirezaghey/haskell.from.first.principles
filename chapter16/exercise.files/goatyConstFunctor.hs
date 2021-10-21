data EvilGoateeConst a b =
  GoatyConst b
  
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)
