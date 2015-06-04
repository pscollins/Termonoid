{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
module Termonoid where

import Graphics.UI.Gtk

import Control.Monad.IO.Class

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Concurrent
import System.Posix.Pty

import TermNetwork
import Terminal


mainAxn :: IO ()
mainAxn = do
  (pty, _) <-
    spawnWithEnv "bash" [] (80, 80)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print
  attrs <- getTerminalAttributes pty
  print $ getAttributes attrs

  setTerminalAttributes pty (attrs `withoutMode` EnableEcho) Immediately

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

  _ <- forkIO $ watch textIn (livePty sessionPty)

  _ <- win `on` keyPressEvent $
       eventKeyVal >>= liftIO . fire keyPress >> return True


  _ <- (textBuf sessionPty) `on` bufferChanged $
       liftIO $ fire bufChange ()

  mainGUI
