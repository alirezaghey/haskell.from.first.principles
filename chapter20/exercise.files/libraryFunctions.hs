import Data.Monoid (Sum(..), Product(..), Any(..))
import Data.Semigroup (Min(..))

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


-- using foldr
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y acc -> y == x) False

-- using foldMap and Any from Data.Monoid
elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x = getAny . foldMap (\y -> Any (x == y))

-- using foldr
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x y -> min <$> Just x <*> y) Nothing


null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True