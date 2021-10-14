

newtype Combine a b =
  Combine {unCombine :: a -> b} 

instance Show (Combine a b) where
  show (Combine _) = "Combine a b"

instance (Semigroup b)
      => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (Monoid b)
    => Monoid (Combine a b) where
      mempty = Combine (const mempty)
      mappend = (<>)
