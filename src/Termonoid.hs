{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
module Termonoid where

import Graphics.UI.Gtk

import System.IO
import Control.Monad.IO.Class
import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Concurrent
import GHC.Word
import System.Posix.Pty

import TermNetwork
import Terminal


mainAxn = do
  (pty, _) <-
    spawnWithEnv "bash" [] (80, 80)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print
  getTerminalAttributes pty >>= print . getAttributes
  attrs <- getTerminalAttributes pty

  initGUI >>= print

  win <- windowNew
  scrolledWin <- scrolledWindowNew Nothing Nothing
  txt <- textViewNew
  textViewSetWrapMode txt WrapWord
  livePty <- mkLivePty pty txt

  containerAdd win scrolledWin
  containerAdd scrolledWin txt

  widgetShowAll win

  (keyPress, textIn) <- (,) <$> newAddHandler <*> newAddHandler
  network <- setupNetwork keyPress textIn livePty
  actuate network

  forkIO $ watch textIn pty


  win `on` keyPressEvent $ do
    k <- eventKeyVal
    liftIO $ fire keyPress k
    liftIO $ print $ keyToChar k
    return True

  mainGUI
