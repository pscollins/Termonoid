{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
module Termonoid where

import System.Posix.Pty
import System.Process
import Data.ByteString.Char8 (pack)

-- | Spawn in my regular env
spawnWithEnv :: FilePath -> [String] ->
                (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithEnv = spawnWithPty Nothing True

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

main :: IO ()
main = do
  (pty, shellHandle) <-
    spawnWithEnv "bash" [] (20, 10)

  getTerminalName pty >>= print
  getSlaveTerminalName pty >>= print

  tryReadPty pty >>= print
  writePty pty $ pack "ls\n"
  tryReadPty pty >>= print
  tryReadPty pty >>= print
  tryReadPty pty >>= print
  tryReadPty pty >>= print
  tryReadPty pty >>= print
  tryReadPty pty >>= print
  tryReadPty pty >>= print

  attrs <- getTerminalAttributes pty >>= return . getAttributes
  print $ attrs
