
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) x Nil = x
  (<>) Nil x = x
  (<>) (Cons a as) xs = Cons a (as <> xs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs <> (<*>) fs xs
