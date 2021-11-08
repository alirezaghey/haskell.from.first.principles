myLiftA2 :: Applicative f =>
            (a -> b -> c) ->
            f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b



newtype Reader r a = Reader {runReader :: r -> a}

asks :: (r -> a) -> Reader r a
asks = Reader