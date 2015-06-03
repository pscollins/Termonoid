{-# LANGUAGE StandaloneDeriving, FlexibleInstances, TemplateHaskell #-}
module Termonoid where

import System.Posix.Terminal
import System.Posix.IO
import System.IO
import GHC.IO.Handle
import System.Process
import Data.ByteString.Char8 (pack)
import Control.Monad
import Control.Lens.TH
import Control.Lens.Setter


makeLensesFor [
  ("cmdspec", "cmdspec'")
  , ("cwd", "cwd'")
  , ("env", "env'")
  , ("std_in", "std_in'")
  , ("std_out", "std_out'")
  , ("std_err", "std_err'")
  , ("close_fds", "close_fds'")
  , ("create_group", "create_group'")
  , ("delegate_ctlc", "delegate_ctlc'")] ''CreateProcess


deriving instance Show BaudRate
deriving instance Show StdStream
deriving instance Show CmdSpec
deriving instance Show CreateProcess
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

data Pty = Pty {master, slave :: Handle}

readPty :: Pty -> IO String
readPty = hGetLine . master

writePty :: Pty -> String -> IO ()
writePty = hPutStr . slave

-- runExecutable :: FilePath -> [String] -> Handle -> IO ProcessHandle
-- runExecutable path args hSlave = do
--   [hStdin, hStdout, hStderr] <- replicateM 3 (Just <$> hDuplicate hSlave)
--   runProcess path args Nothing Nothing hStdin hStdout hStderr

mkCreateProcess :: Pty -> String -> CreateProcess
mkCreateProcess (Pty m s) toRun = CreateProcess {


main :: IO ()
main = do
  (m, s) <- openPseudoTerminal
  m' <- fdToHandle m
  s' <- fdToHandle s
  let pty = Pty m' s'
  -- ph <- runExecutable "bash" [] s'
  writePty pty "ls"
  readPty pty >>= print
  -- (pty, shellHandle) <-
  --   spawnWithEnv "bash" [] (20, 10)

  -- getTerminalName pty >>= print
  -- getSlaveTerminalName pty >>= print

  -- tryReadPty pty >>= print
  -- writePty pty $ pack "ls\n"
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print
  -- tryReadPty pty >>= print

  -- attrs <- getTerminalAttributes pty >>= return . getAttributes
  -- print $ attrs
