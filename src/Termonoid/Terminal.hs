module Terminoid.Terminal where

-- | Spawn in my regular env
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



watch :: EventSource PtyOut -> Pty -> IO ()
watch textIn pty = forever $ do
  got <- readPty pty
  fire textIn got
  threadWaitReadPty pty
  return ()


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
