module Mu.Commands.Core where

import Mu.Types
import Mu.Commands.Display
import Mu.Commands.Info
import Mu.Commands.Input
import Mu.Commands.State

-- | The super-basic editor commands, fundamental
--   for doing anything.
coreCommands :: [Command]
coreCommands = [ startInputCmd
               , getInputCmd
               , changeStateCmd
               , setInfoCmd
               , displayCmd
               ]
