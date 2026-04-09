module Main where

import Data.List.NonEmpty (NonEmpty (..))
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
        MyLib.tellCar Car {company = "toyota", model = "plius", year = (2525 :: Int)}
          `shouldBe` "this car \"plius\" from \"toyota\"was made in 2525"

      it "solveRpn" $
        map MyLib.solveRpn ["1 2 3 + 4 - +", "10 4 3 + 2 * -", "1 2", "5", "1 + 2 3", "%"]
          `shouldBe` [ Right (2 :: Double),
                       Right (-4 :: Double),
                       Left $ InvalidSyntax NumberRemain,
                       Right (5 :: Double),
                       Left $ InvalidSyntax SignRemain,
                       Left UnparsableToken
                     ]

  describe "london" $
    do
      it "owo" $
        MyLib.heathrowToLondon [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]
          `shouldBe` Right
            MyLib.Route
              { nodeHistory =
                  Node B 4
                    :| [ Node B 3,
                         Node B 2,
                         Node A 2,
                         Node A 1,
                         Node B 1,
                         Node B 0
                       ],
                dist = 75 :: Int
              }
      it "partial1" $
        MyLib.heathrowToLondon [50, 10, 0]
          `shouldBe` Right
            MyLib.Route
              { nodeHistory =
                  Node B 1
                    :| [ Node B 0
                       ],
                dist = 10 :: Int
              }
      it "partial2" $
        MyLib.heathrowToLondon [50, 10, 30, 5, 90, 0]
          `shouldBe` Right
            MyLib.Route
              { nodeHistory =
                  Node A 2
                    :| [ Node A 1,
                         Node B 1,
                         Node B 0
                       ],
                dist = 45 :: Int
              }
      it "partial3" $
        MyLib.heathrowToLondon [50, 10, 30, 5, 90, 20, 40, 2, 0]
          `shouldBe` Right
            MyLib.Route
              { nodeHistory =
                  Node B 3
                    :| [ Node B 2,
                         Node A 2,
                         Node A 1,
                         Node B 1,
                         Node B 0
                       ],
                dist = 67 :: Int
              }
      it "partial3" $
        MyLib.heathrowToLondon [50, 10, 30, 5, 90, 20, 40, 2, 1, 2, 1, 0]
          `shouldBe` Right
            MyLib.Route
              { nodeHistory =
                  Node B 4
                    :| [ Node B 3,
                         Node B 2,
                         Node A 2,
                         Node A 1,
                         Node B 1,
                         Node B 0
                       ],
                dist = 67 :: Int
              }
