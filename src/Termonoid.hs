{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
module Termonoid where

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

-- type PtyOut = Either [PtyControlCode] ByteString
type PtyOut = ByteString

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


setupNetwork :: EventSource KeyVal -> EventSource PtyOut -> Pty
                -> TextBuffer -> IO EventNetwork
setupNetwork keyPress textIn pty buf = compile $ do
  ePressed <- fromAddHandler $ addHandler keyPress
  eText <- fromAddHandler $ addHandler textIn

  -- let fromPtyOut :: PtyOut -> String = either (\_ -> "") show
  -- let eGood = apply (pure show) eText

  let eCharable = filterJust $ keyToChar <$> ePressed
      ePrintableChars = filterE (`elem` ['A'..'z']) eCharable
      eEnter = filterE (== '\n') eCharable
      bufAppend :: [Char] -> IO ()
      bufAppend = textBufferInsertAtCursor buf
      bufAppendC c = bufAppend [c]
      -- and now we want to grab the last lines + push them down to the shell

  reactimate $ fmap print ePressed
  reactimate $ fmap print eText

  reactimate $ bufAppendC <$> ePrintableChars
  reactimate $ bufAppend <$> (apply (pure unpack) eText)


watch :: EventSource PtyOut -> Pty -> IO ()
watch textIn pty = forever $ do
  got <- readPty pty
  fire textIn got
  threadWaitReadPty pty
  return ()


deriving instance Show BaudRate
-- deriving instance Show (Either [PtyControlCode] ByteString)

data Attributes = Attributes { inSpeed :: BaudRate
                             , outSpeed :: BaudRate
                             , bitsInByte :: Int
                             , inTime :: Int
                             , minIn :: Int } deriving Show
                             -- , termMode :: TerminalMode}

getAttributes :: TerminalAttributes -> Attributes
getAttributes = Attributes <$>
                inputSpeed <*>
                outputSpeed <*>
                bitsPerByte <*>
                inputTime <*>
                minInput


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
