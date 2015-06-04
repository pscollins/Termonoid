{-# LANGUAGE StandaloneDeriving #-}
module Terminal where

import Graphics.UI.Gtk
import System.Posix.Pty
import System.Process
import Control.Applicative
import Data.ByteString.Char8 (pack, ByteString)
import System.Glib.UTFString

import ParserTypes

spawnWithEnv :: FilePath -> [String] ->
                (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithEnv = spawnWithPty Nothing True


deriving instance Show BaudRate

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

data LivePty = LivePty { livePty :: Pty
                       , textView :: TextView
                       , textBuf :: TextBuffer
                       , endMark :: TextMark
                       }


mkEndMark' :: TextBuffer -> TextView -> Bool -> IO TextMark
mkEndMark' buf tv gravity = do
  endIter <- textBufferGetEndIter buf
  textBufferCreateMark buf Nothing endIter gravity

mkEndMark :: LivePty -> Bool -> IO TextMark
mkEndMark (LivePty _ tv tb _) = mkEndMark' tb tv

mkLivePty :: Pty -> TextView -> IO LivePty
mkLivePty pty tv = do
  buf <- textViewGetBuffer tv
  end <- mkEndMark' buf tv False

  return LivePty { livePty = pty
                 , textView = tv
                 , textBuf = buf
                 , endMark = end }

writeExpr :: LivePty -> [DisplayExpr] -> IO ()
writeExpr pty = undefined

buffAppend :: LivePty -> String -> IO ()
buffAppend pty = textBufferInsertAtCursor (textBuf pty) . stringToGlib

buffAppend' :: LivePty -> Char -> IO ()
buffAppend' pty = (buffAppend pty) . (:[])

buffAppendBS :: LivePty -> ByteString -> IO ()
buffAppendBS pty = textBufferInsertByteStringAtCursor (textBuf pty)

writeConsole :: LivePty -> String -> IO ()
writeConsole pty = writePty (livePty pty) . pack

scrollTo :: LivePty -> TextMark -> IO ()
scrollTo pty = textViewScrollMarkOnscreen (textView pty)

emptyGlib :: DefaultGlibString
emptyGlib = stringToGlib ""


colorTag :: TextTag -> ColorCmd -> IO ()
colorTag attrs Reset = set attrs [ textTagBackground := emptyGlib
                                 , textTagForeground := emptyGlib ]
colorTag attrs (Set (col, pos)) = set attrs [ (getter pos) := newColor ]
  where getter :: ColorPos -> WriteAttr TextTag String
        getter Foreground = textTagForeground
        getter Background = textTagBackground
        newColor = show  col

-- | We're going to apply formatting in two steps: first, walk a long
-- the body of text we want to insert and drop marks every time we
-- see a formatting code that we need to take care of.
dropTags :: LivePty -> [DisplayExpr] -> IO [TextMark]
dropTags pty = mapM dropTag
  where dropTag = undefined

writeParsed :: LivePty -> [DisplayExpr] -> IO ()
writeParsed = undefined
-- writeParsed pty (Text s) = buffAppend pty s
