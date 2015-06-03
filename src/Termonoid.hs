module Termonoid where

import System.Posix.Pty
import System.Process
import Data.ByteString.Char8 (pack)

import Graphics.UI.Gtk
import System.IO
import Control.Monad.IO.Class

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

main = do
  (pty, _) <-
    spawnWithEnv "bash" [] (20, 10)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print

  initGUI >>= print

  win <- windowNew
  widgetShowAll win

  let writeMe = writePty pty
  let readMe = readPty pty

  win `on` keyPressEvent $ do
    k <- eventKeyVal
    liftIO $ print $ keyToChar k
    liftIO $ writePty' pty $ kvToS k
    liftIO $ readPty pty >>= return . print
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
