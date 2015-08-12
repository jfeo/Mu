module Mu.Core.Buffers where

import Data.Default
import Mu.API.Types

-- Default editor buffers --

foreground :: Color
foreground = (5, 5, 5)

background :: Color
background = (0, 0, 2)

defaultAttributes :: [Attr]
defaultAttributes = [Foreground foreground, Background background]

coreBuffers :: [Buffer]
coreBuffers = [mainBuf, statusBuf, commandBuf]

mainBufName :: String
mainBufName = "main"

mainBuf :: Buffer
mainBuf = def { bufName = mainBufName }

statusBufName :: String
statusBufName = "status"

statusBuf :: Buffer
statusBuf = def { bufName = statusBufName
                , bufDefAttr = defaultAttributes
                }

commandBufName ::String
commandBufName = "command"

commandBuf :: Buffer
commandBuf = def { bufName = commandBufName 
                 , bufDefAttr = defaultAttributes
                 }
