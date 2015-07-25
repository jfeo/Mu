module Main where

import Mu.Display
import Mu.Input
import Mu.State
import Mu.Types
import Mu.Utils
import Data.Default

m = "main"
mainBuf = def { bufPos = (0, 0),
                bufText = "smiler til kings",
                bufAttr = [( 1, 5, [Foreground (5, 0, 2)]),
                           (10, 5, [Foreground (2, 0, 5)])] }
firstEd = def { edActive = m }

loop :: Editor -> IO ()
loop ed = do
    -- fix
    ed1 <- getInput ed
    let ed2 = changeState ed1
    display ed2
    loop ed2

main :: IO ()
main = do
    startInput
    (w, h) <- termSize
    let ed1  = addBuffer m mainBuf firstEd
        ed2  = changeBuffer m ed1 $ \b ->
          b { bufSize = (w, h-2) }
    loop ed2
