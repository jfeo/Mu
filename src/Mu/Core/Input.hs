-- | Defines /core/ commands related to retriving user input from
--   stdin. As of now there is limited support for modifier keys
--   and no support for mouse interaction at all; this should be 
--   fixed. A binding to the "libtermkey" c library could be a solution.
module Mu.Core.Input ( startInputPart
                     , startInput
                     , getInputPart
                     , getInput
                     ) where

import Mu.Types
import Mu.Part
import Data.Default
import System.IO

-- | Prepares the program for input by sensible input
--   configuration.
--   Default command level is 0.
startInputPart :: Part
startInputPart = def { partName  = "core_startInput"
                     , partMoment = -25
                     , partFun   = startInput
                     , partStarted = True
                     }

-- | The /main/ command for getting user input. Parsed
--   keyboard input is inserted into the 'edInput' field.
--   Default command level is 4.
getInputPart :: Part
getInputPart = def { partName  = "core_getInput"
                   , partMoment = 0
                   , partFun   = getInput
                   , partStarted = True
                   }

-- | Parses raw input into corresponding 'Input'.
parseInput :: String -> Input
parseInput ""           = None
parseInput "\t"         = Chars "  "
parseInput "\ESC"       = Escape
parseInput "\DEL"       = Backspace
parseInput "\ESC[3~"    = Delete
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
parseInput s            = Chars $ filter (\c -> not $ c `elem` nonchar) s

-- | Characters to remove from 'Chars' during 'parseInput'.
nonchar :: [Char]
nonchar = "\ESC"

-- | Characters denoting the start of an escape sequence.
waitOn :: [Char]
waitOn = "\ESC["

-- | Retrieves the remaining part of an escape sequence.
--   Timeout is 5 ms.
getRest :: IO String
getRest = do
    w <- hWaitForInput stdin 2
    if w then do
        c <- getChar
        s <- getRest
        return $ c : s
    else do
        return ""

-- | Gets raw input from 'stdin'. Including full escape
--   sequences.
getRaw :: IO String
getRaw = do
  c <- hGetChar stdin
  r <- case c `elem` waitOn of
         True  -> getRest
         False -> return ""
  return $ c : r

-- | Sets suitabe input settings for a text-editor.
startInput :: Part -> Mu -> IO Mu
startInput _ mu = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    return $ stopPart "core_startInput" mu

-- | Gets input from the keyboard and updates
--   the muInput field of the muitor.
getInput :: Part -> Mu -> IO Mu
getInput _ mu = do
    s <- getRaw
    let p = parseInput s
    return $ mu { muInput = p }
