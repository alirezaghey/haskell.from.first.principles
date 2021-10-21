data IgnoreOne f g a b =
  IgnoreingSomething (f a) (g b) deriving (Eq, Show)


instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreingSomething fa gb) = IgnoreingSomething fa (fmap f gb)