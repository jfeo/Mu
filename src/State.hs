module Mu.State (
    changeState
)where

import Mu.Types
import Mu.Utils

changeState :: Editor -> IO Editor
changeState ed = return $ change (edState ed) (edInput ed) ed

change :: State -> Input -> Editor -> Editor
change Insert (Chars s) ed =
  changeBuffer "main" ed $ \b ->
    b { bufText = inter (bufCurs b) s (bufText b),
        bufCurs = 1 + bufCurs b }

change Insert Delete ed = 
  changeBuffer "main" ed $ \b ->
    let c = bufCurs b
     in b { bufText = ripOut (c-1) $ bufText b,
            bufCurs = max 0 (c-1) }

change Insert LeftArr ed = 
  changeBuffer "main" ed $ \b ->
    let c = bufCurs b
     in b { bufCurs = max 0 (c-1) }

change Insert RightArr ed =
  changeBuffer "main" ed $ \b ->
    let c = bufCurs b
        l = length $ bufText b
     in b { bufCurs = min l (c+1) }

change _ _ ed = ed
