newtype Comp a =
  Comp (a -> a)


instance Show (Comp a) where
  show (Comp _) = "Comp a"

instance (Semigroup a)
      => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid a
    => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

