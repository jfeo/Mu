module Mu.Input (
    startInput,
    getInput  
)where

import Mu.Types
import System.IO

parseInput :: String -> Input
parseInput ""           = None
parseInput "\t"         = Chars "  "
parseInput "\ESC"       = Escape
parseInput "\DEL"       = Delete
parseInput "\ESC[A"     = UpArr
parseInput "\ESC[B"     = DownArr
parseInput "\ESC[C"     = RightArr
parseInput "\ESC[D"     = LeftArr
parseInput "\ESC[1;2A"  = SUpArr
parseInput "\ESC[1;2B"  = SDownArr
parseInput "\ESC[1;2C"  = SRightArr
parseInput "\ESC[1;2D"  = SLeftArr
parseInput "\ESC\ESC[A" = MUpArr
parseInput "\ESC\ESC[B" = MDownArr
parseInput "\ESC\ESC[C" = MRightArr
parseInput "\ESC\ESC[D" = MLeftArr
parseInput s            = Chars s 

waitOn = "\ESC["

getRest :: IO String
getRest = do
    w <- hWaitForInput stdin 5
    if w then do
        c <- getChar
        s <- getRest
        return $ c : s
    else do
        return ""

getRaw :: IO String
getRaw = do
    c <- hGetChar stdin
    r <- case c `elem` waitOn of
           True  -> getRest
           False -> return ""
    return $ c : r

startInput :: IO ()
startInput = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering

getInput :: Editor -> IO Editor
getInput ed = do
    s <- getRaw
    let p = parseInput s
    return $ ed { edInput = p }