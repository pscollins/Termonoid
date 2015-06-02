module Termonoid where

import System.Posix.Pty
import System.Process

-- | Spawn in my regular env
spawnWithEnv :: FilePath -> [String] ->
                (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithEnv = spawnWithPty Nothing True

main :: IO ()
main = do
  (pty, shellHandle) <-
    spawnWithEnv "ls" ["-l"] (20, 10)

  getTerminalName pty >>= putStrLn
  getSlaveTerminalName pty >>= putStrLn
