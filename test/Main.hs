module Main (main) where

import Parser

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "primitive parser" $ do
    it "parses the empty string" $ do
      parsePrim "" `shouldBe` [""]
    it "parses a normal string" $ do
      parsePrim "abcde" `shouldBe` [PrimText "abcde"]
    it "recognizes a control sequence" $ do
      parsePrim "\x1b[0;m" `shouldBe` [PrimControlSeq "[0;m"]
    it "recognizes both together" $ do
      parsePrim "abc\x1b[0;m" `shouldBe` [PrimText "abc"
                                         , PrimControlSeq "[0;m"]
