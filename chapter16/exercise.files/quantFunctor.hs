data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)