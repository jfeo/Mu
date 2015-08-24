module Mu.Core.Buffers where

import Data.Default
import Mu.Types

-- Default editor buffers --

foreground :: Color
foreground = C256 255

background :: Color
background = C256 23

defaultLook :: [Look]
defaultLook = [Foreground foreground, Background background]

coreBuffers :: [Buffer]
coreBuffers = [mainBuf, statusBuf, commandBuf]

mainBufName :: String
mainBufName = "main"

mainBuf :: Buffer
mainBuf = def { bufName    = mainBufName
			  , bufDefLook = [ Foreground $ C256 16
			                 , Background $ C256 255
			                 ] 
			  }

statusBufName :: String
statusBufName = "status"

statusBuf :: Buffer
statusBuf = def { bufName    = statusBufName
                , bufDefLook = defaultLook
                }

commandBufName ::String
commandBufName = "command"

commandBuf :: Buffer
commandBuf = def { bufName    = commandBufName 
                 , bufDefLook = defaultLook
                 }
