{-# LANGUAGE StandaloneDeriving #-}
module Terminal where

import Graphics.UI.Gtk
import System.Posix.Pty
import System.Process
import Control.Applicative
import Data.ByteString.Char8 (pack, ByteString, unpack)


spawnWithEnv :: FilePath -> [String] ->
                (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithEnv = spawnWithPty Nothing True

writePty' :: Pty -> String -> IO ()
writePty' pty s = writePty pty $ pack s

kvToS :: KeyVal -> String
kvToS kv = case keyToChar kv of
            Just c -> [c]
            Nothing -> []

type PtyOut = ByteString

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
