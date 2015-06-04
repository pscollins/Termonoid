{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module TermNetwork where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Posix.Pty
import Control.Monad
import Data.ByteString.Char8 (ByteString, unpack)
import System.Glib.UTFString (stringToGlib)
import Data.Char (ord)

import Terminal
import Parser

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data KbdEvents t = KbdEvents { alphaNum :: Event t Char
                             , clear :: Event t Char
                             , del :: Event t Char }

watch :: EventSource ByteString -> Pty -> IO ()
watch textIn pty = forever $
  readPty pty >>= fire textIn >> threadWaitReadPty pty

lineToSend :: KbdEvents t -> Event t String
lineToSend KbdEvents {alphaNum, clear, del} =
  accumE "" $ ((cons <$> alphaNum)
               `union`
               (reset <$> clear)
               `union`
               (back <$> del))
  where cons = (:)
        reset _ = const ""
        back _ (s:ss) = ss
        back _ [] = []


-- We'd like to make this more general, but oh well
eventPairs :: Event t String -> Event t (String, String)
eventPairs ev = accumE ("", "") (adv <$> ev)
  where adv new (_, old) = (old, new)


rightEmpty :: (String, String) -> Maybe String
rightEmpty (s, "") = Just s
rightEmpty _ = Nothing

fixUp :: String -> String
fixUp  = reverse . ('\n':)

-- This is too restrictive, but let's use it for now
-- Be aware that control keys tend to not actually have char representations
mkKbdEvents :: Event t KeyVal -> KbdEvents t
mkKbdEvents eText = KbdEvents { alphaNum = filterJust $ keyToChar <$> eText
                              , clear = '\n' <$ filterE (== returnVal) eText
                              , del = '^' <$ filterE (== delVal) eText }
  where returnVal = keyFromName $ stringToGlib "Return"
        delVal = keyFromName $ stringToGlib "BackSpace"


-- mkScrollEvent :: LivePty -> EventSource () -> Event t () -> IO (Event t TextMark)
-- mkScrollEvent (LivePty {livePty, textView, textBuf}) bufChanged eChanged = do




--     return (endMark <$ eChanged)


setupNetwork :: EventSource KeyVal -> EventSource ByteString ->
                EventSource () -> LivePty -> IO EventNetwork
setupNetwork keyPress textIn bufChanged pty = compile $ do
  ePressed <- fromAddHandler $ addHandler keyPress
  eText <- fromAddHandler $ addHandler textIn
  eChanged <- fromAddHandler $ addHandler bufChanged

  let kbdEvents = mkKbdEvents ePressed
      doSend = lineToSend kbdEvents
      fullLines = fixUp  <$> (filterJust $ rightEmpty <$> eventPairs doSend)




  -- let fromPtyOut :: PtyOut -> String = either (\_ -> "") show
  -- let eGood = apply (pure show) eText

  -- DEBUG
  reactimate $ print <$> ePressed
  reactimate $ print . keyName <$> ePressed
  reactimate $ print <$> doSend
  reactimate $ print <$> eventPairs doSend
  reactimate $ print <$> fullLines
  reactimate $ print <$> eText
  -- reactimate $ print . map ord . unpack <$> eText



  -- REAL LIFE
  reactimate $ buffAppend' pty <$> (alphaNum kbdEvents `union` clear kbdEvents)
  reactimate $ (const $ killOne pty) <$> del kbdEvents
  reactimate $ writeParsed pty . parse . unpack <$> eText
  reactimate $ writeConsole pty <$> fullLines
  reactimate $ scrollTo pty <$> (endMark pty <$ eChanged)



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
