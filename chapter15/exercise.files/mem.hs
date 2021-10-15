newtype Mem s a =
  Mem {
    runMem :: s -> (a, s) 
} 

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) =
    Mem $ \s ->
      let (a', s') = g s
          (a'', s'') = f s'
       in (a'' <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)