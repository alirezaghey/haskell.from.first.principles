import Data.Monoid (Sum(..), Product(..))

-- using foldr
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- using foldMap and Sum from Data.Monoid
sum2 :: (Foldable t, Num a) => t a -> a
sum2 xs = getSum $ foldMap Sum xs

-- eta reduced
sum3 :: (Foldable t, Num a) => t a -> a
sum3 = getSum . foldMap Sum


-- using foldr
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

-- using foldMap and Product from Data.Monoid
product2 :: (Foldable t, Num a) => t a -> a
product2 xs = getProduct $ foldMap Product xs

-- eta reduce
product3 :: (Foldable t, Num a) => t a -> a
product3 = getProduct . foldMap Product