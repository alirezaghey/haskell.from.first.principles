data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)


instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read x)    = Read (fmap f x)