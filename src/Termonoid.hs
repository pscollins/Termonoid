{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
module Termonoid.Termonoid where

import System.Posix.Pty
import System.Process
import Data.ByteString.Char8 (pack, ByteString, unpack)

import Graphics.UI.Gtk
import System.IO
import Control.Monad.IO.Class
import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Concurrent
import GHC.Word

import Termonoid.Network
import Termonoid.Terminal


main = do
  (pty, _) <-
    spawnWithEnv "bash" [] (80, 80)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print
  getTerminalAttributes pty >>= print . getAttributes
  attrs <- getTerminalAttributes pty

  initGUI >>= print

  win <- windowNew
  txt <- textViewNew
  txtBuf <- textViewGetBuffer txt

  containerAdd win txt

  widgetShowAll win

  (keyPress, textIn) <- (,) <$> newAddHandler <*> newAddHandler
  network <- setupNetwork keyPress textIn pty txtBuf
  actuate network

  forkIO $ watch textIn pty


  win `on` keyPressEvent $ do
    k <- eventKeyVal
    liftIO $ fire keyPress k
    liftIO $ print $ keyToChar k
    -- liftIO $ writePty' pty $ kvToS k
    -- liftIO $ readPty pty >>= return . print
    return True
    -- readMe >>= print

  mainGUI
