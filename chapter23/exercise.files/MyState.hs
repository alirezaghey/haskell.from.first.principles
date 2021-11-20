module MyState where

newtype MyState s a = MyState {runMyState :: s -> (a, s)}

instance Functor (MyState s) where
  fmap f (MyState g) = MyState $ \s -> let (a,  ns) = g s
                                        in (f a, ns)

instance Applicative (MyState s) where
  pure x = MyState $ \s -> (x, s)
  (MyState f) <*> (MyState g) = MyState $ \s -> let (fun, ns) = f s
                                                    (a, ms) = g s
                                                in  (fun a, ms)

instance Monad (MyState s) where
  return = pure
  (MyState x) >>= f = MyState $ \s -> let (a, ns) = x s
                                          (MyState g) = f a
                                          (b, sg) = g ns
                                      in  (b, sg)

get :: MyState s s
get = MyState $ \s -> (s, s)