{-# LANGUAGE StandaloneDeriving, TupleSections #-}
module Terminal where

import Graphics.UI.Gtk
import System.Posix.Pty
import System.Process
import Control.Applicative
import Data.ByteString.Char8 (pack, ByteString)
import System.Glib.UTFString
import Control.Monad
import Data.Maybe (catMaybes)

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

-- | Note that "left gravity" = false means "falls to the right,"
-- i.e. follows insertions. Left gravity = true means does not follow
-- insertions.
mkEndMark' :: TextBuffer -> TextView -> Bool -> IO TextMark
mkEndMark' buf tv gravity = do
  endIter <- textBufferGetEndIter buf
  textBufferCreateMark buf Nothing endIter gravity

mkEndMark :: LivePty -> Bool -> IO TextMark
mkEndMark (LivePty _ tv tb _) = mkEndMark' tb tv

mkIter :: LivePty -> TextMark -> IO TextIter
mkIter pty = textBufferGetIterAtMark (textBuf pty)

mkLivePty :: Pty -> TextView -> IO LivePty
mkLivePty pty tv = do
  buf <- textViewGetBuffer tv
  end <- mkEndMark' buf tv False

  return LivePty { livePty = pty
                 , textView = tv
                 , textBuf = buf
                 , endMark = end }

tagToEnd :: LivePty -> TextIter -> TextTag -> IO ()
tagToEnd pty start tag = endIter >>= textBufferApplyTag buf tag start
  where buf = textBuf pty
        endIter = mkIter pty $ endMark pty

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

colorTag' :: ColorCmd -> IO (TextTag)
colorTag' cmd = do
  tag <- textTagNew Nothing
  colorTag tag cmd
  return tag

-- | We're going to apply formatting in three steps: first, walk along
-- the body of text we want to insert and drop marks every time we
-- see a formatting code that we need to take care of.
dropMarks :: LivePty -> [DisplayExpr] -> IO [Maybe ([ColorCmd], TextMark)]
dropMarks pty = mapM dropMark
  where dropMark :: DisplayExpr -> IO (Maybe ([ColorCmd], TextMark))
        dropMark (Text s) = buffAppend pty s >> return Nothing
        dropMark (SGR cmds) = Just <$> (mkEndMark pty True >>= return . (,) cmds)

-- | Second, we replace our marks with tags to format the document.
-- Colors are the only kind of formatting that needs tags.
dropTags :: [Maybe ([ColorCmd], TextMark)] -> IO [([TextTag], TextMark)]
dropTags = mapM dropTag . catMaybes
  where dropTag (cmds, mark) = mapM colorTag' cmds >>= return . (, mark)

applyTags :: LivePty -> [([TextTag], TextMark)] -> IO ()
applyTags pty = mapM_ doTags
  where doTags (tags, mark) = do
          start <- mkIter pty mark
          mapM_ (tagToEnd pty start) tags


writeParsed :: LivePty -> [DisplayExpr] -> IO ()
writeParsed pty exprs = dropMarks' exprs >>= dropTags >>= applyTags'
  where dropMarks' = dropMarks pty
        applyTags' = applyTags pty
-- writeParsed pty (Text s) = buffAppend pty s
