module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 :: Integer) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 :: Integer) + 2 `shouldBe` 4
  describe "division" $ do
    it "15 divided by 3 is 5)" $ do
      dividedBy (15 :: Integer) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy (22 :: Integer) 5 `shouldBe` (4, 2)

sayHello  :: IO ()
sayHello  = putStrLn "hello!"


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0 where
  go  n   d   count
    | n < d     = (count, n)
    | otherwise = go (n-d) d (count+1)

