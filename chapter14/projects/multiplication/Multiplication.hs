module Multiplication where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "13 * 2 is equal to 26" $ do
      multiplyBySum (13 :: Integer) 2 `shouldBe` 13 * 2
    it "2 * 13 is equal to 26" $ do
      multiplyBySum (2 :: Integer) 13 `shouldBe` 2 * 13
    it "3 * 4 is equal to 12" $ do
      multiplyBySum (3 :: Integer) 4 `shouldBe` 3 * 4

multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum _ 0 = 0
multiplyBySum 0 _ = 0
multiplyBySum x y = x + multiplyBySum x (y-1)