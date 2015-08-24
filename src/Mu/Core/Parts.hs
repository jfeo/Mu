module Mu.Core.Parts where

import Mu.Types
import Mu.Core.Input
import Mu.Core.State
import Mu.Core.Display
import Mu.Core.Info

-- | The super-basic editor commands, fundamental
--   for doing anything.
coreParts :: [Part]
coreParts = [ startInputPart
            , getInputPart
            , changeStatePart
            , setInfoPart
            , displayPart
            ]
