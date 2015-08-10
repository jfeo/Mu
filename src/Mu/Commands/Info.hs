-- | Updates features of the editor, such as
--   the sizes of the buffers.
module Mu.Commands.Info ( setInfoCmd
                        , setInfo
                        ) where

import Data.Default
import Mu.Outlib
import Mu.Types
import Mu.Utils

-- | Updates the "xMax" and "yMax" variables, and sets
--   the sizes of the three main buffers.
setInfoCmd :: Command
setInfoCmd = def { cmdName    = "core_info"
                 , cmdLevel   = 12
                 , cmdFun     = setInfo
                 , cmdStarted = True
                 }

-- | See 'setInfoCmd'.
setInfo :: Editor -> IO Editor
setInfo ed = do
  (termW,termH) <- termSize
  return $ showVar "xMax" termW
         $ showVar "xMax" termH 
         $ setSize statusBuf termW 1
         $ setPos  statusBuf 0 0
         $ setSize mainBuf termW (termH-2)
         $ setPos  mainBuf 0 1
         $ setSize commandBuf termW 1
         $ setPos  commandBuf 0 (termH-1)
         $ ed
