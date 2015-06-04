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
  sessionPty <- mkLivePty pty txt

  containerAdd win scrolledWin
  containerAdd scrolledWin txt

  scrolledWindowSetPolicy scrolledWin PolicyNever PolicyAutomatic

  widgetShowAll win

  (keyPress, textIn, bufChange) <-
    (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
  network <- setupNetwork keyPress textIn bufChange sessionPty
  actuate network

  forkIO $ watch textIn (livePty sessionPty)

  win `on` keyPressEvent $ do
    k <- eventKeyVal
    liftIO $ fire keyPress k
    liftIO $ print $ keyToChar k
    return True

  (textBuf sessionPty) `on` bufferChanged >> liftIO $ fire bufChange ()

  mainGUI
