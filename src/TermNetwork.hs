{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module TermNetwork where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Posix.Pty
import Control.Monad
import Data.ByteString.Char8 (ByteString, unpack)
import System.Glib.UTFString (stringToGlib)
import Control.Concurrent.Chan

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


watchKeys :: EventSource KeyVal -> Chan KeyVal -> IO ()
watchKeys keyPress chan = forever $
  readChan chan >>= fire keyPress >> putStrLn "did a read"

lineToSend :: KbdEvents t -> Event t String
lineToSend KbdEvents {alphaNum, clear, del} =
  accumE "" $ ((cons <$> alphaNum)
               `union`
               (reset <$> clear)
               `union`
               (back <$> del))
  where cons = (:)
        reset _ = const ""
        back _ (_:ss) = ss
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


  let reactimateSafe :: Frameworks t => Event t (IO ()) -> Moment t ()
      reactimateSafe  = reactimate . fmap postGUIAsync

  -- REAL LIFE
  reactimateSafe $  buffAppend' pty <$>
   (alphaNum kbdEvents `union` clear kbdEvents)
  reactimateSafe $ (const $ killOne pty) <$> del kbdEvents
  reactimateSafe $ writeParsed pty . parse . unpack <$> eText
  reactimateSafe $ writeConsole pty <$> fullLines
  reactimateSafe $ scrollTo pty <$> (endMark pty <$ eChanged)
  reactimateSafe $ const (postGUIAsync mainQuit) <$>
    (filterE (== 'Q') $ alphaNum kbdEvents)



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
