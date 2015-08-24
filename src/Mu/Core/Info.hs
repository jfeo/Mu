-- | Updates features of the editor, such as
--   the sizes of the buffers.
module Mu.Core.Info ( setInfoPart
                        , setInfo
                        ) where

import Data.Default
import Mu.Types
import Mu.Buffer
import Mu.Var
import Mu.Core.Buffers
import Mu.Core.Outlib

-- | Updates the "xMax" and "yMax" variables, and sets
--   the sizes of the three main buffers.
setInfoPart :: Part
setInfoPart = def { partName    = "core_info"
                  , partMoment  = 75
                  , partFun     = setInfo
                  , partStarted = True
                  }

-- | See 'setInfoPart'.
setInfo :: Part -> Mu -> IO Mu
setInfo _ mu = do
  (termW,termH) <- termSize
  return $ showVar "xMax" termW
         $ showVar "yMax" termH 
         $ changeBuffer statusBufName
            (\b -> setPos 0 0 
                 $ setSize termW 1 b
            )
         $ changeBuffer mainBufName
            (\b -> setPos 0 1 
                 $ setSize termW (termH-2) b
            )
         $ changeBuffer commandBufName
            (\b -> setPos 0 (termH - 1) 
                 $ setSize termW 1 b
            )
         $ mu
