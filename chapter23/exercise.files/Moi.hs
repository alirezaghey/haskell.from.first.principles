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