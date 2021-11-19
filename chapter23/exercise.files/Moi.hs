{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x -> let (y, z) = g x in
                                   (f y, z)
  