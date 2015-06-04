module Main (main) where

import Parser

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "primitive parser" $ do
    it "parses the empty string" $ do
      parsePrim "" `shouldBe` []
    it "parses a normal string" $ do
      parsePrim "abcde" `shouldBe` [PrimText "abcde"]
    it "recognizes a control sequence" $ do
      parsePrim "\x1b[0;m" `shouldBe` [PrimControlSeq "[0;m"]
    it "recognizes both together" $ do
      parsePrim "abc\x1b[0;m" `shouldBe` [ PrimText "abc"
                                         , PrimControlSeq "[0;m" ]

  describe "parser" $ do
    it "parses empty" $ do
      parse "" `shouldBe` []
    it "parses a normal string" $ do
      parse "abcde" `shouldBe` [Text "abcde"]
    it "recognizes a control sequence" $ do
      parse "\x1b[0;m" `shouldBe` [SGR [Reset]]
    it "recognizes both together" $ do
      parse "abc\x1b[0;m" `shouldBe` [ Text "abc"
                                     , SGR [Reset] ]
    it "recognizes color settings" $ do
      parse "\x1b[30;41;m" `shouldBe` [SGR [ Set (Black, Foreground)
                                           , Set (Red, Background) ]]
