{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module TermNetwork where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import System.Posix.Pty
import Control.Monad
import Data.ByteString.Char8 (pack, ByteString, unpack)
import System.Glib.UTFString (stringToGlib)
import Data.Char (ord)

import Terminal

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data KbdEvents t = KbdEvents { alphaNum :: Event t Char
                             , clear :: Event t Char }

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


-- We'd like to make this more general, but oh well
eventPairs :: Event t String -> Event t (String, String)
eventPairs ev = accumE ("", "") (adv <$> ev)
  where adv new (older, old) = (old, new)


rightEmpty :: (String, String) -> Maybe String
rightEmpty (s, "") = Just s
rightEmpty _ = Nothing

fixUp :: String -> String
fixUp  = reverse . ('\n':)

-- This is too restrictive, but let's use it for now
-- Be aware that control keys tend to not actually have char representations
mkKbdEvents :: Event t KeyVal -> KbdEvents t
mkKbdEvents eText = KbdEvents { alphaNum = filterJust $ keyToChar <$> eText
                              , clear = '\n' <$ filterE (== returnVal) eText }
  where returnVal = keyFromName $ stringToGlib "Return"

setupNetwork :: EventSource KeyVal -> EventSource PtyOut -> Pty
                -> TextBuffer -> IO EventNetwork
setupNetwork keyPress textIn pty buf = compile $ do
  ePressed <- fromAddHandler $ addHandler keyPress
  eText <- fromAddHandler $ addHandler textIn

  let kbdEvents = mkKbdEvents ePressed
      doSend = lineToSend kbdEvents
      fullLines = fixUp  <$> (filterJust $ rightEmpty <$> eventPairs doSend)
      bufAppend = textBufferInsertAtCursor buf . stringToGlib
      bufAppendC = bufAppend  . (:[])
  -- let fromPtyOut :: PtyOut -> String = either (\_ -> "") show
  -- let eGood = apply (pure show) eText

  -- DEBUG
  reactimate $ print <$> ePressed
  reactimate $ print . keyName <$> ePressed
  reactimate $ print <$> doSend
  reactimate $ print <$> eventPairs doSend
  reactimate $ print <$> fullLines
  reactimate $ print <$> eText
  reactimate $ print . map ord . unpack <$> eText

  -- REAL LIFE
  reactimate $ bufAppendC <$> alphaNum kbdEvents
  reactimate $ textBufferInsertByteStringAtCursor buf <$> eText
  reactimate $ writePty pty . pack <$> fullLines



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
