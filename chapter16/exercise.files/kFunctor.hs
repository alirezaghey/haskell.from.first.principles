data K a b =
  K a

instance Functor (K a) where
  fmap _ (K x) = K x