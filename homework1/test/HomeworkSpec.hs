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

        it "handles odd lists" $ do
            doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

    describe "sumDigits" $ do
        it "adds up all of the digits" $ do
            sumDigits [16,7,12,5] `shouldBe` 22

    describe "validate" $ do
        it "validates a number that's remainder is 0" $ do
            validate 4012888888881881 `shouldBe` True

        it "doesn't validate a number that's remainder is not 0" $ do
            validate 4012888888881882 `shouldBe` False

    describe "hanoi" $ do
        it "works for 2 disks on the tower" $ do
          hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]

main :: IO ()
main = hspec spec
