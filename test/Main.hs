module Main (main) where

import Parser
import ParserTypes

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
      parse "\x1b[0m" `shouldBe` [SGR [Reset]]
    it "recognizes both together" $ do
      parse "abc\x1b[0m" `shouldBe` [ Text "abc"
                                     , SGR [Reset] ]
    it "recognizes color settings" $ do
      parse "\x1b[30;41m" `shouldBe` [SGR [ Set (Black, Foreground)
                                           , Set (Red, Background) ]]
    it "recognizes other settings" $ do
      parse "\x1b[1J" `shouldBe` [ CSI 'J' ["1"] ]

  describe "control seq" $ do
    it "makes a CSI" $ do
      mkControlSeq 'J' ["1"] `shouldBe` CSI 'J' ["1"]
  describe "SGR" $ do
    it "makes reset" $ do
      mkColorCmd "0" `shouldBe` Reset
    it "makes bg and fg" $ do
      mkColorCmd "30" `shouldBe` Set (Black, Foreground)
