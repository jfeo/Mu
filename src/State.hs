module Mu.State (
    changeState  
)where

import Mu.Types
import Mu.Utils

changeState :: Editor -> Editor
changeState ed = change (edState ed) (edInput ed) ed

change :: State -> Input -> Editor -> Editor
change Insert (Chars s) ed =
   changeBuffer "main" ed $ \b ->
     b { bufText = inter (1 + bufCurs b) s (bufText b),
         bufCurs = 1 + bufCurs b }
