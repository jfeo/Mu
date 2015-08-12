module Main where

import Mu.API.Command
import Mu.API.Types
import Mu.Core.Buffers
import Mu.Core.Commands
import Mu.Core.Shipped
import Data.Default

-- | Runs thrugh and executes all commands, in level
--   order from n to m, starting from l. Once.
--   Commands of a late level, started by an earlier level
--   command, will be executed the same round.
runLevels :: Int -> Int -> Int -> Editor -> IO Editor
runLevels l n m ed = do
    let cmds = filter cmdStarted
             $ filter (\ c -> cmdLevel c == l)
             $ edCommands ed
    ed' <- run cmds ed
    if (l + 1) > m then return ed'
                   else runLevels (l + 1) n m ed'

-- | Loops through all commands - in order - forever.
loop :: Editor -> IO ()
loop ed = do
    let levels = map cmdLevel $ edCommands ed
        minL   = minimum levels
        maxL   = maximum levels
    ed' <- runLevels minL minL maxL ed
    loop ed'

-- | The main method!
main :: IO ()
main = do 
    let ed = def { edActive   = mainBufName
                 , edBuffers  = coreBuffers
                 , edCommands = coreCommands
                             ++ shippedCommands
                 , edVars = [ ("mainbuffer", mainBufName)
                            , ("commandBuffer", commandBufName)
                            , ("statusBuffer", statusBufName)
                            ]
                 }
    loop $ ed
