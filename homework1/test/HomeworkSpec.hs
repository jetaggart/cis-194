module HomeworkSpec where

import Test.Hspec
import Homework

spec :: Spec
spec = do
    describe "toDigits" $ do
        it "turns an integer into an array of ints" $ do
            toDigits 1234 `shouldBe` [1,2,3,4]

        it "handles 0" $ do
            toDigits 0 `shouldBe` []

        it "handles negative numbers" $ do
            toDigits (-17) `shouldBe` []

    describe "toDigitsRev" $ do
        it "turns an integer into the reversed array of ints" $ do
            toDigitsRev 1234 `shouldBe` [4,3,2,1]

    describe "doubleEveryOther" $ do
        it "doubles each integer starting from the right" $ do
            doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

main :: IO ()
main = hspec spec
