module CIS194.HW10.ApplicativeSpec where

import           CIS194.HW10.AParser
import           CIS194.HW10.Applicative
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "abParser" $ do
    it "abcdef" $
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")

    it "aebcdf" $
      runParser abParser "aebcd" `shouldBe` Nothing

  describe "abParser_" $ do
    it "abcdef" $
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")

    it "aebcdf" $
      runParser abParser_ "aebcd" `shouldBe` Nothing

  describe "intPair" $
    it "12 34" $
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

  describe "intOrUppercase" $ do
    it "342abcd" $
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")

    it "XYZ" $
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")

    it "foo" $
      runParser intOrUppercase "foo" `shouldBe` Nothing
