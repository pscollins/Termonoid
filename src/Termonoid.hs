module Termonoid where

import Graphics.UI.Gtk
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = do
  initGUI >>= print

  win <- windowNew
  f <- openFile "foo" WriteMode

  -- let print = hPutStrLn f

  widgetShowAll win

-- THESE DONT DO ANYTHING, DONT KNOW WHY
  -- on win mapSignal (print "hello world")
  -- on win showSignal (print "hello world")
  -- on win focus (\_ -> print "hello world" >> return True)

  win `on` buttonPressEvent $ do
    liftIO $ print "hi"
    return True

  mainGUI
  mainQuit
