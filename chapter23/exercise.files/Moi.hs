{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x -> let (y, z) = g x in
                                   (f y, z)
  

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  -- pure a = Moi $ \s -> (a, s)
  -- Using TupleSection (it's the same as above)
  pure a = Moi (a, )
  
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (fun, ns) = f s
                                        (arg, fs) = g ns
                                    in  (fun arg, fs)


instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, sf) = f s
                                  (Moi h) = g a
                                  (b, sg) = h sf
                              in  (b, sg)