-- | Commands for drawing the muitor to the screen.
module Mu.Core.Display ( display
                       , displayPart
                       ) where

import Data.Default
import Mu.Buffer
import Mu.Types
import Mu.Core.Outlib

-- | This is the core command which draw the muitor on
--   your console screen. All Lookibute-related commands
--   should be executed before this.
--   Default command level is 16.
displayPart :: Part
displayPart = def { partName    = "core_display"
                  , partMoment  = 100
                  , partFun     = display
                  , partStarted = True
                  }

-- | See 'displayPart'.
display :: Part -> Mu -> IO Mu
display _ mu = do
  let bufs = muBuffers mu
      act  = muActive mu
  utfs <- sequence $ map (\ b -> displayBuffer b) bufs
  putUtfStr $ concat utfs
  case getBuffer act mu of
    Nothing  -> fail $ "Could not find main buffer: " ++ act
    Just buf -> do
       let (cX, cY) = indexXY buf bufCurs
           (_, oY)  = indexXY buf bufOff
           (pX, pY) = bufPos buf
           (x, y)   = (pX + cX, pY + cY - oY)
       putUtfStr $ curSet x y
       putUtfStr "\0"
  return mu

-- | Displays a single buffer.
displayBuffer :: Buffer -> IO String
displayBuffer b = do
    let t = bufText b
        o = bufOff b
        a = bufLook b
        f = bufDefLook b
        (w, h) = bufSize b
        (xMin, yMin) = bufPos b
    (termW, termH) <- termSize
    let xMax = min (xMin + w) termW
        yMax = min (yMin + h) termH
        utf = displayText xMin xMax yMax t a f xMin yMin o
        set = curSet xMin yMin
        fot = escLook f
    return $ set ++ fot ++ utf ++ reset

-- | Draws text in a box defined by 'xMin', 'xMax' and 'yMax,
--   starting from the current cursor position, which must
--   be set manually by the 'x' and 'y' arguments.
--   To actually draw, the returned text must be written to 
--   the terminal.
displayText :: Int        -- ^ The minimal x coord (line start).
            -> Int        -- ^ The maximal x coord.
            -> Int        -- ^ The maximal y coord (last line).
            -> String     -- ^ The text to display.
            -> [TextLook] -- ^ The Lookibutes for the text.
            -> [Look]     -- ^ The Lookibutes to default to.
            -> Int        -- ^ The current x position.
            -> Int        -- ^ The current y position.
            -> Int        -- ^ The current text index.
            -> String
displayText xMin xMax yMax text look footer x y i
  | i >= l = "" ++ pad ++ linePad
  | y >= yMax = "" ++ pad
  | x >= xMax = aesc ++ pad ++ nextLine i
  | c == '\n' = aesc ++ pad ++ nextLine (i + 1)
  | otherwise = aesc ++ [c] ++ rec (x + 1) y (i + 1)
  where c = text !! i
        l = length text
        rec = displayText xMin xMax yMax text look footer
        nextLine = \ j -> curSet xMin (y + 1) ++ rec xMin (y + 1) j
        pad = replicate (xMax - x) ' '
        linePad = unlines
                $ replicate (yMax - y - 1)
                $ replicate (xMax - xMin) ' '
        aesc = case (filter (\ tl -> lookStart tl == i) look,
                     filter (\ tl -> lookEnd   tl == i) look) of
                 (sa, []) -> escLook $ concat $ map looks sa
                 (_ , _)  -> escLook footer

-- | Creates a string of escape sequences for the
--   giv Lookibtues.
escLook :: [Look] -> String
escLook [] = ""
escLook ((Foreground (RGB r g b)) : as) = (rgbFg r g b) ++ escLook as
escLook ((Background (RGB r g b)) : as) = (rgbBg r g b) ++ escLook as
escLook ((Foreground (C256 c)) : as) = colorFg c ++ escLook as
escLook ((Background (C256 c)) : as) = colorBg c ++ escLook as
escLook (Reset : as) = reset ++ escLook as
escLook ((Custom s) : as) = s ++ escLook as
