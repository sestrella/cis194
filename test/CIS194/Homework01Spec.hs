module CIS194.Homework01Spec where

import           CIS194.Homework01
import           Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts positive integers to a list of digits" $
      toDigits 1234 `shouldBe` [1, 2, 3, 4]

    it "returns an empty list for 0" $
      toDigits 0 `shouldBe` []

    it "returns an empty list for negative inputs" $
      toDigits (-17) `shouldBe` []

  describe "toDigitsRev" $
    it "converts positive integers to a list of digits reversed" $
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

  describe "doubleEveryOther" $
    it "doubles every other number beginning from the right" $
      -- doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
      pending
