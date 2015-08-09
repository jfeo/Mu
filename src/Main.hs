module Main where

import Mu.Display
import Mu.Input
import Mu.State
import Mu.Types
import Mu.Utils
import Data.Default

color1 :: Color
color1 = (5,5,5)

color2 :: Color
color2 = (1,2,0)

loop :: Editor -> IO ()
loop ed = do
    ed'  <- getInput ed
    ed'' <- changeState ed'
    _    <- display ed''
    loop ed''

main :: IO ()
main = do
    (w, h) <- termSize
    let at = [(0,w,[Foreground color1,
                    Background color2])]
        st = def { bufSize = (w, 1),
                   bufText = "Mu Editor",
                   bufAttr = at }
        mn = def { bufPos  = (0, 1),
                   bufSize = (w, h-2),
                   bufAttr = [] }
        cm = def { bufPos  = (0, h-1),
                   bufSize = (w, 1),
                   bufText = "command-bar",
                   bufAttr = at }
        ed = def { edActive = mainBuf,
                   edBuffers = [("status",  st),
                                ("main",    mn),
                                ("command", cm)]}
    startInput
    loop ed
