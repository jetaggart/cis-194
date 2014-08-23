
module Homework2.LogAnalysisSpec where

import Test.Hspec
import Homework2.Log
import Homework2.LogAnalysis

spec :: Spec
spec = do

  describe "parseMessage" $ do
    it "parses an error message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "parses an info message" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "parses an unknown message" $ do
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

  describe "parse" $ do
    it "parses the first line of error.log correctly" $ do
      result <- testParse parse 1 "data/error.log"
      result `shouldBe` [LogMessage Info 5053 "con ing!"]

main :: IO ()
main = hspec spec
