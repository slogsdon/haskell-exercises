module UtilSpec ( spec ) where

import Helper
import Util

spec :: Spec
spec = do
  describe "maximum'" $ do
    it "1" $ do
      (maximum' [1] :: Int) `shouldBe` (1 :: Int)

    it "1 to 2" $ do
      (maximum' [1,2] :: Int) `shouldBe` (2 :: Int)

    it "1 to 3" $ do
      (maximum' [1,3,2] :: Int) `shouldBe` (3 :: Int)