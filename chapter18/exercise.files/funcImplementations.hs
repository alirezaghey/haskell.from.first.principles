import Control.Monad (join)
import Data.Functor ((<&>))

j :: Monad m => m (m a) -> m a
j x = x >>= id


{-
Different ways of implementing l1
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= (return . f)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x <&> f
-}
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap