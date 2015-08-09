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

startCommand :: String -> Editor -> Maybe Editor
startCommand s e =
  case catMaybes $ map (\ (n, c) -> enc n $ (cmdParse c) s) $ edCommands e of
    []      -> Nothing
    (n,a):_ -> Just $ change n e $ \ c -> 
                        c { cmdStarted = True,
                            cmdArgs    = a }
  where enc _ Nothing  = Nothing
        enc n (Just a) = Just (n,a)

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
