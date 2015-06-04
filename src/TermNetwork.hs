{-# LANGUAGE NamedFieldPuns #-}
module TermNetwork where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import System.Posix.Pty
import Control.Monad
import Data.ByteString.Char8 (pack, ByteString, unpack)

import Terminal

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data KbdEvents t = KbdEvents { alphaNum :: Event t Char
                              ,  clear :: Event t () }

watch :: EventSource PtyOut -> Pty -> IO ()
watch textIn pty = forever $ do
  got <- readPty pty
  fire textIn got
  threadWaitReadPty pty
  return ()


lineToSend :: KbdEvents t -> Event t String
lineToSend KbdEvents {alphaNum, clear} =
  accumE "" $ (cons <$> alphaNum) `union` (reset <$> clear)
  where cons = (:)
        reset _ = const ""


-- This is too restrictive, but let's use it for now
mkKbdEvents :: Event t KeyVal -> KbdEvents t
mkKbdEvents eText = KbdEvents { alphaNum = filterE (`elem` ['A'..'z']) eChars
                              , clear = () <$ filterE (== '\n') eChars }
  where eChars = filterJust $ keyToChar <$> eText

setupNetwork :: EventSource KeyVal -> EventSource PtyOut -> Pty
                -> TextBuffer -> IO EventNetwork
setupNetwork keyPress textIn pty buf = compile $ do
  ePressed <- fromAddHandler $ addHandler keyPress
  eText <- fromAddHandler $ addHandler textIn

  let kbdEvents = mkKbdEvents ePressed
      doSend = lineToSend kbdEvents
  -- let fromPtyOut :: PtyOut -> String = either (\_ -> "") show
  -- let eGood = apply (pure show) eText

  -- DEBUG
  reactimate $ print <$> ePressed
  reactimate $ print <$> doSend
  -- let eCharable = filterJust $ keyToChar <$> ePressed
  --     ePrintableChars = filterE (`elem` ['A'..'z']) eCharable
  --     eEnter = filterE (== '\n') eCharable
  --     bufAppend :: [Char] -> IO ()
  --     bufAppend = textBufferInsertAtCursor buf
  --     bufAppendC c = bufAppend [c]
  --     -- eGetToPrint = accumE "" ((fmap (:) ePrintableChars)
  --     --                          `union`
  --     --                          (fmap (const "") eEnter))
  --     -- and now we want to grab the last lines + push them down to the shell

  -- reactimate $ fmap print ePressed
  -- reactimate $ fmap print eText
  -- reactimate $ fmap print eGetToPrint
  -- reactimate $ bufAppendC <$> ePrintableChars
  -- reactimate $ bufAppend <$> (apply (pure unpack) eText)
