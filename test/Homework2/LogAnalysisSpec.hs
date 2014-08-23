
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
      head result `shouldBe` LogMessage Info 5053 "pci_id: con ing!"

    it "parses the second line of error.log correctly" $ do
      (_:result:_) <- testParse parse 2 "data/error.log"
      
      result `shouldBe` LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
  
    it "parses a warning line of error.log correctly" $ do
      (_:_:result:_) <- testParse parse 3 "data/error.log"
      
      result `shouldBe` LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"

main :: IO ()
main = hspec spec
