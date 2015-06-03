module Termonoid where

import System.Posix.Pty
import System.Process
import Data.ByteString.Char8 (pack, ByteString)

import Graphics.UI.Gtk
import System.IO
import Control.Monad.IO.Class
import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Concurrent
import GHC.Word

-- | Spawn in my regular env
spawnWithEnv :: FilePath -> [String] ->
                (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithEnv = spawnWithPty Nothing True

writePty' :: Pty -> String -> IO ()
writePty' pty s = writePty pty $ pack s

kvToS :: KeyVal -> String
kvToS kv = case keyToChar kv of
            Just c -> [c]
            Nothing -> []


type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


setupNetwork :: EventSource KeyVal -> EventSource Word -> IO EventNetwork
setupNetwork keyPress textIn = compile $ do
  ePressed <- fromAddHandler $ addHandler keyPress
  eText <- fromAddHandler $ addHandler textIn

  reactimate $ fmap print ePressed
  reactimate $ fmap print eText


watch :: EventSource ByteString -> Pty -> IO ()
watch textIn pty = forever $ do
  got <- readPty pty
  fire textIn got

main = do
  (pty, _) <-
    spawnWithEnv "bash" [] (20, 10)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print

  initGUI >>= print

  win <- windowNew
  widgetShowAll win

  (keyPress, textIn) <- (,) <$> newAddHandler <*> newAddHandler
  network <- setupNetwork keyPress textIn

  forkIO $ watch textIn pty


  actuate network

  win `on` keyPressEvent $ do
    k <- eventKeyVal
    liftIO $ fire keyPress k
    liftIO $ print $ keyToChar k
    liftIO $ writePty' pty $ kvToS k
    -- liftIO $ readPty pty >>= return . print
    return True
    -- readMe >>= print

  mainGUI

  -- tryReadPty pty >>= print
  -- writePty pty $ pack "ls\n"
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print

  -- attrs <- getTerminalAttributes pty >>= return . getAttributes
  -- print $ attrs
