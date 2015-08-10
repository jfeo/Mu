module Main where

import Mu.Commands
import Mu.Commands.Core
import Mu.Types
import Data.Default
import Data.List (sortBy)

color1 :: Color
color1 = (5, 5, 5)

color2 :: Color
color2 = (0, 2, 1)

loop :: Editor -> IO ()
loop ed = do
    let cmds = sortBy (\ c1 c2 -> (cmdLevel c1) `compare` (cmdLevel c2))
             $ filter cmdStarted
             $ edCommands ed
    ed' <- run cmds ed
    loop ed'

main :: IO ()
main = do 
    let as = [Foreground color1, Background color2]
        bs = [(mainBuf, def),
              (statusBuf,  def {bufDefAttr = as}),
              (commandBuf, def {bufDefAttr = as})]
    let ed = def { edActive   = mainBuf,
                   edBuffers  = bs,
                   edCommands = coreCommands}
    loop $ ed
