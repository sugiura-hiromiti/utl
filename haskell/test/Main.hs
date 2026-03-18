module Main where

import MyLib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "aaa" $
    do
      it "aaa" $
        abs (-3) `shouldBe` (3 :: Double)

      it "divByTen" $
        MyLib.divByTen 20 `shouldBe` (2 :: Double)

      it "numLongChains" $
        MyLib.numLongChains `shouldBe` 1

      it "application" $
        MyLib.application `shouldBe` (4 :: Double)

      it "negAbs" $
        map MyLib.negAbs [9, -3, 1, 5, -9] `shouldBe` ([-9, -3, -1, -5, -9] :: [Int])

      it "car" $
        MyLib.tellCar Car{company = "toyota", model = "plius", year = (2525 :: Int)} `shouldBe` "this car \"plius\" from \"toyota\"was made in 2525"
