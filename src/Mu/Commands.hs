module Mu.Commands (
  startCommand,
  parseWord,
  run,
  shippedCommands
)where

import Mu.Types
import Mu.Utils
import Data.Default
import Data.Maybe (catMaybes)
import System.Exit

-- | Starts commands whoose 'cmdParse' functions returns
--   a 'Just' value for the input string.
startCommand :: String -> Editor -> Editor
startCommand s e = e { edCommands = map (startIfParse s)
                                  $ edCommands e } 

startIfParse :: String -> Command -> Command
startIfParse s c = case (cmdParse c) s of
                     Nothing -> c
                     Just a  -> c { cmdStarted = True
                                  , cmdArgs    = a
                                  }

run :: [Command] -> Editor -> IO Editor
run [] e     = return e
run (c:cs) e = do
    e' <- cmdFun c e
    run cs e'

parseWord :: String -> (String -> Maybe [String])
parseWord p = \s -> if s == p then Just []
                              else Nothing

shippedCommands :: [(String, Command)]
shippedCommands = [("quit", quitCmd)]

quitFun :: Editor -> IO Editor
quitFun ed = do
  _ <- exitSuccess
  return ed

quitCmd :: Command
quitCmd = def { cmdLevel = 9,
                cmdParse = parseWord "quit",
                cmdFun   = quitFun }
