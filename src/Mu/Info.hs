module Mu.Info (
  setInfo
) where

import Mu.Types
import Mu.Utils
import Mu.Outlib
import Data.Time

setInfo :: Editor -> IO Editor
setInfo ed = do
  (termW,termH) <- termSize
  time <- getZonedTime
  let timeString = justCenter termW
                 $ formatTime defaultTimeLocale "%d/%m/%y - %H:%M:%S" time
  return $ showVar "xMax" termW
         $ showVar "xMax" termH 
         $ setSize statusBuf termW 1
         $ setPos  statusBuf 0 0
         $ setText statusBuf timeString
         $ setSize mainBuf termW (termH-2)
         $ setPos  mainBuf 0 1
         $ setSize commandBuf termW 1
         $ setPos  commandBuf 0 (termH-1)
         $ ed
