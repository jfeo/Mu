module Mu.Core.Commands where

import Mu.API.Types
import Mu.Core.Input
import Mu.Core.State
import Mu.Core.Display
import Mu.Core.Info

-- | The super-basic editor commands, fundamental
--   for doing anything.
coreCommands :: [Command]
coreCommands = [ startInputCmd
               , getInputCmd
               , changeStateCmd
               , setInfoCmd
               , displayCmd
               ]
