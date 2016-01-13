module CIS194.HW10.EX01Spec where

import           Test.Hspec

spec :: Spec
spec =
  describe "fmap" $
    it "identity" $ do
      let f = undefined
      pure id <*> Parser f `shouldBe` Parser f
