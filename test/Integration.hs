module Main where

import Test.Hspec

import Graphics.UI.Gtk
import Control.Concurrent

import Terminal

withTextView :: (TextView -> a) -> IO a
withTextView f = do
  initGUI
  win <- windowNew
  scrolledWin <- scrolledWindowNew Nothing Nothing
  txt <- textViewNew
  textViewSetWrapMode txt WrapWord
  containerAdd win scrolledWin
  containerAdd scrolledWin txt
  scrolledWindowSetPolicy scrolledWin PolicyNever PolicyAutomatic
  widgetShowAll win
  forkIO mainGUI
  return $ f txt





main :: IO ()
main = hspec $ do
  describe "mock textview" $ do
    it "runs" $ withTextView $ \tv ->
      ()
