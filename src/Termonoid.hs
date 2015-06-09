{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
module Termonoid where

import Graphics.UI.Gtk

import Control.Monad.IO.Class
import Control.Monad
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

  putStrLn "about to set up stuff"

  win <- windowNew
  scrolledWin <- scrolledWindowNew Nothing Nothing
  txt <- textViewNew
  textViewSetWrapMode txt WrapWord

  putStrLn "about to make pty"

  sessionPty <- mkLivePty pty txt

  putStrLn "made the UI objects"

  containerAdd win scrolledWin
  containerAdd scrolledWin txt

  scrolledWindowSetPolicy scrolledWin PolicyNever PolicyAutomatic

  widgetShowAll win

  putStrLn "about to set up network"

  (keyPress, textIn, bufChange) <-
    (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
  network <- setupNetwork keyPress textIn bufChange sessionPty
  actuate network

  keyChan <- newChan

  _ <- forkIO $ watch textIn (livePty sessionPty)
  _ <- forkIO $ watchKeys keyPress keyChan

  _ <- win `on` keyPressEvent $
       eventKeyVal >>= liftIO . writeChan keyChan >> return True


  _ <- (textBuf sessionPty) `on` bufferChanged $
       liftIO $ fire bufChange ()

  putStrLn "all setup"

  mainGUI
