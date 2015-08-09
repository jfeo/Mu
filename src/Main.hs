module Main where

import Mu.Commands
import Mu.Display
import Mu.Info
import Mu.Input
import Mu.State
import Mu.Types
import Data.Default
import Data.List (sortBy)

color1 :: Color
color1 = (5, 5, 5)

color2 :: Color
color2 = (0, 0, 1)

loop :: Editor -> IO ()
loop ed = do
    let cmds = sortBy (\ c1 c2 -> (cmdLevel c1) `compare` (cmdLevel c2))
             $ filter cmdStarted
             $ map snd
             $ edCommands ed
    ed' <- run cmds ed
    loop ed'

main :: IO ()
main = do
    let input = def { cmdLevel   = 4,
                      cmdFun     = getInput,
                      cmdStarted = True }
        state = def { cmdLevel   = 8,
                      cmdFun     = changeState,
                      cmdStarted = True }
        info = def { cmdLevel   = 12,
                     cmdFun     = setInfo,
                     cmdStarted = True }
        draw = def { cmdLevel   = 16,
                     cmdFun     = display,
                     cmdStarted = True }
        cs = [("core_input", input), 
              ("core_state", state),
              ("core_info", info),
              ("core_display", draw)]
        as = [Foreground color1, Background color2]
        bs = [(mainBuf, def),
              (statusBuf,  def {bufDefAttr = as}),
              (commandBuf, def {bufDefAttr = as})]
    let ed = def { edActive   = mainBuf,
                   edBuffers  = bs,
                   edCommands = cs ++ shippedCommands}
    startInput
    loop $ ed
