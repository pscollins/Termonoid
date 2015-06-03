module Network where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler


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
      eGetToPrint = accumE "" (fmap (:) ePrintableChars)
                               -- `union`
                               -- (fmap (\_ -> \_ -> "") eEnter))
      -- and now we want to grab the last lines + push them down to the shell

  reactimate $ fmap print ePressed
  reactimate $ fmap print eText
  reactimate $ fmap print eGetToPrint
  reactimate $ bufAppendC <$> ePrintableChars
  reactimate $ bufAppend <$> (apply (pure unpack) eText)
