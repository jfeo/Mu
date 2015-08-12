module Mu.API.Command ( run
                      , startCommands
                      , parseWord
                      , parseWordArgs
                      , withCommand
                      , getArgs
                      , startCommand
                      , stopCommand
                      ) where

import Mu.API.Types

-- | Starts commandVs whoose 'cmdParse' functions returns
--   a 'Just' value for the input string.
startCommands :: String -> Editor -> Editor
startCommands s e = e { edCommands = map (startIfParse s)
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

parseWordArgs :: String -> (String -> Maybe [String])
parseWordArgs p = \ s -> 
  case words s of
    []     -> Nothing
    (n:as) -> if n == p then Just as
                        else Nothing
withCommand :: String -> Editor -> (Command -> a) -> Maybe a
withCommand name ed f = 
  case name `look` edCommands ed of
    Nothing -> Nothing
    Just c  -> Just $ f c 

getArgs :: String -> Editor -> [String]
getArgs name ed = maybe [] id
                $ withCommand name ed cmdArgs

startCommand :: String -> Editor -> Editor
startCommand name ed =
    change name ed $ \ c ->
      c {cmdStarted = True}

stopCommand :: String -> Editor -> Editor
stopCommand name ed = 
    change name ed $ \ c ->
      c { cmdStarted = False }
