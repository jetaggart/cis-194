
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
         

main :: IO ()
main = hspec spec
