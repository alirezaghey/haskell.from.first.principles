import Data.Monoid (Sum(..))

-- using foldr
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- using foldMap and Sum from Data.Monoid
sum2 :: (Foldable t, Num a) => t a -> a
sum2 xs = getSum $ foldMap Sum xs

-- eta reduced
sum3 :: (Foldable t, Num a) => t a -> a
sum3 = getSum . foldMap Sum