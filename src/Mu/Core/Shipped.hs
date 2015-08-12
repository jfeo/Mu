module Mu.Core.Shipped where

import Data.Default
import System.Exit

import Mu.API.Utils
import Mu.API.Command
import Mu.API.Types
import Mu.Core.Buffers

shippedCommands :: [Command]
shippedCommands = [openCmd, quitCmd]

quitCmd :: Command
quitCmd = def { cmdName  = "quit"
              , cmdLevel = 9
              , cmdParse = parseWord "quit"
              , cmdFun   = quit
              }

quit :: Editor -> IO Editor
quit ed = do
  _ <- exitSuccess
  return ed

openCmd :: Command
openCmd = def { cmdName  = "open"
              , cmdLevel = 9
              , cmdParse = parseWordArgs "open"
              , cmdFun   = open
              }

open :: Editor -> IO Editor
open ed =
  let this = cmdName openCmd in
  case getArgs this ed of
    []   -> return
          $ stopCommand this
          $ setText commandBufName "No filename given!" ed
    (f:_) -> do
        c <- readFile f
        return $ setVar "filename" f
               $ setText commandBufName
                   ("'" ++ f ++ "' successfully loaded")
               $ setText mainBufName c
               $ stopCommand this ed
