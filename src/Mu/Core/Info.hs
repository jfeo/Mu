-- | Updates features of the editor, such as
--   the sizes of the buffers.
module Mu.Core.Info ( setInfoCmd
                        , setInfo
                        ) where

import Data.Default
import Mu.API.Types
import Mu.API.Utils
import Mu.Core.Buffers
import Mu.Core.Outlib

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
         $ setSize statusBufName termW 1
         $ setPos  statusBufName 0 0
         $ setSize mainBufName termW (termH-2)
         $ setPos  mainBufName 0 1
         $ setSize commandBufName termW 1
         $ setPos  commandBufName 0 (termH-1)
         $ ed
