module Main (main) where

import Parser
import ParserTypes

import System.Glib.UTFString
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
    it "strips cr" $ do
      parsePrim "\r\n" `shouldBe` [PrimText "\n"]
    it "strips cr with other things" $ do
      parsePrim "abc\r\ndef" `shouldBe` [PrimText "abc\ndef"]
  -- TODO: OSC tests on processPrim
  describe "OSC parser" $ do
    it "rejects the empty string" $ do
      mkOSCmd "" `shouldBe` []
    it "supports code 1" $ do
      mkOSCmd "1;hello world" `shouldBe` [WindowTitle "hello world"]
    -- it "supports \\a terminator" $ do
    --   mkOSCmd "1;hello world;" `shouldBe` [WindowTitle "hello world"]
    it "supports code 2" $ do
      mkOSCmd "2;hello world" `shouldBe` [IconName "hello world"]
    it "supports code 0" $ do
      mkOSCmd "0;hi" `shouldBe` [IconName "hi", WindowTitle "hi"]

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

    it "works on real output" $ do
      parse "\x1b[0m\x1b[01;34m.git\x1b[0m\r\ndrwxr-xr-x" `shouldBe`
        [ SGR [Reset], SGR [Bold, Set (Blue, Foreground) ]
        , Text ".git", SGR [Reset], Text "\ndrwxr-xr-x"]

    it "strips CR" $ do
      parse "\r\n" `shouldBe` [Text "\n"]

    it "works on real output with colors" $ do
      parse "\x1b[01;34m.\x1b[0m\r\ndrwxr-xr-x" `shouldBe` [
        SGR [ Bold
            , Set (Blue, Foreground) ]
        , Text "."
        , SGR [Reset]
        , Text "\ndrwxr-xr-x" ]

  describe "control seq" $ do
    it "makes a CSI" $ do
      mkControlSeq 'J' ["1"] `shouldBe` Just (CSI 'J' ["1"])
  describe "SGR" $ do
    it "makes reset" $ do
      mkColorCmd "0" `shouldBe` Just Reset
    it "makes bg and fg" $ do
      mkColorCmd "30" `shouldBe` Just (Set (Black, Foreground))
    it "goes to string property" $ do
      colorForGtk Blue `shouldBe` "blue"
