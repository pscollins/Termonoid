module Termonoid where

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana
import Reactive.Banana.Frameworks


main :: IO ()
main = do
    displayHelpMessage
    sources <- (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
    network <- setupNetwork sources
    actuate network
    eventLoop sources network

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "Commands are:":
    "   count   - send counter event":
    "   pause   - pause event network":
    "   actuate - actuate event network":
    "   quit    - quit the program":
    "":
    []

-- Read commands and fire corresponding events
eventLoop :: (EventSource (), EventSource (), EventSource EventNetwork)
          -> EventNetwork -> IO ()
eventLoop (escounter, esclear,  espause) network = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        case s of
            "count"   -> fire escounter ()
            "clear"   -> fire esclear ()
            "pause"   -> fire espause network
            "actuate" -> actuate network
            "quit"    -> return ()
            _         -> putStrLn $ s ++ " - unknown command"
        when (s /= "quit") loop

-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


-- Set up the program logic in terms of events and behaviors.
setupNetwork :: (EventSource (), EventSource(),
                 EventSource EventNetwork) ->
                IO EventNetwork
setupNetwork (escounter, esclear, espause) = compile $ do
    ecounter <- fromAddHandler (addHandler escounter)
    epause   <- fromAddHandler (addHandler espause  )
    eclear   <- fromAddHandler (addHandler esclear )

    let ecount = accumE "" (((++"1") <$ ecounter)
                            `union` ((++"2") <$ eclear))


    reactimate $ fmap print ecount
    reactimate $ fmap pause epause
