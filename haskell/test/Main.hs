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
        abs (-3) `shouldBe` 3

      it "divByTen" $
        MyLib.divByTen 20 `shouldBe` 2

      it "numLongChains" $
        MyLib.numLongChains `shouldBe` 1

      it "application" $
        MyLib.application `shouldBe` 4

      it "application" $
        map MyLib.negAbs [9, -3, 1, 5, -9] `shouldBe` [-9, -3, -1, -5, -9]

      it "car" $
        MyLib.tellCar Car{company = "toyota", model = "plius", year = 2525} `shouldBe` ""
