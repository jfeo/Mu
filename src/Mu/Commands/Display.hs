-- | Commands for drawing the editor to the screen.
module Mu.Commands.Display ( display
                           , displayCmd
                           ) where

import Data.Default
import Mu.Types
import Mu.Utils
import Mu.Outlib

-- | This is the core command which draw the editor on
--   your console screen. All attribute-related commands
--   should be executed before this.
--   Default command level is 16.
displayCmd :: Command
displayCmd = def { cmdName    = "core_display"
                 , cmdLevel   = 16
                 , cmdFun     = display
                 , cmdStarted = True
                 }

-- | See 'displayCmd'.
display :: Editor -> IO Editor
display ed = do
  clearScreen
  let bufs = edBuffers ed
      act  = edActive ed
  sequence_ $ map (\ (_, b) -> displayBuffer b) bufs
  case getBuffer act ed of
    Nothing -> fail $ "Could not find main buffer: " ++ act
    Just b -> do
       let (k, r) = cursTwo b
           (x, y) = bufPos b
           (u, v) = (k + x, r + y)
       putUtfStr $ curSet u v
       putUtfStr "\0"
  return ed

-- | Displays a single buffer.
displayBuffer :: Buffer -> IO ()
displayBuffer b = do
    let t = bufText b
        o = bufOff b
        a = bufAttr b
        f = bufDefAttr b
        (w, h) = bufSize b
        (xMin, yMin) = bufPos b
    (termW, termH) <- termSize
    let xMax = min (xMin + w) termW
        yMax = min (yMin + h) termH
        utf = displayText xMin xMax yMax t a f xMin yMin o
        set = curSet xMin yMin
        fot = escAttr f
    putUtfStr $ set ++ fot ++ utf ++ reset

-- | Draws text in a box defined by 'xMin', 'xMax' and 'yMax,
--   starting from the current cursor position, which must
--   be set manually by the 'x' and 'y' arguments.
--   To actually draw, the returned text must be written to 
--   the terminal.
displayText :: Int        -- ^ The minimal x coord (line start).
            -> Int        -- ^ The maximal x coord.
            -> Int        -- ^ The maximal y coord (last line).
            -> String     -- ^ The text to display.
            -> [TextAttr] -- ^ The attributes for the text.
            -> [Attr]     -- ^ The attributes to default to.
            -> Int        -- ^ The current x position.
            -> Int        -- ^ The current y position.
            -> Int        -- ^ The current text index.
            -> String
displayText xMin xMax yMax text attr footer x y i
  | i >= l = "" ++ pad ++ linePad
  | y >= yMax = "" ++ pad
  | x >= xMax = aesc ++ pad ++ nextLine i
  | c == '\n' = aesc ++ pad ++ nextLine (i + 1)
  | otherwise = aesc ++ [c] ++ rec (x + 1) y (i + 1)
  where c = text !! i
        l = length text
        rec = displayText xMin xMax yMax text attr footer
        nextLine = \ j -> curSet xMin (y + 1) ++ rec xMin (y + 1) j
        pad = replicate (xMax - x) ' '
        linePad = unlines
                $ replicate (yMax - y - 1)
                $ replicate (xMax - xMin) ' '
        aesc = case (filter (\ (j, _, _) -> j == i) attr,
                     filter (\ (_, k, _) -> k == i) attr) of
                 (sa, []) -> escAttr $ concat $ map (\ (_, _, a) -> a) sa
                 (_ , _)  -> escAttr footer

-- | Creates a string of escape sequences for the
--   giv attribtues.
escAttr :: [Attr] -> String
escAttr [] = ""
escAttr ((Foreground (r, g, b)) : as) = (rgbFg r g b) ++ escAttr as
escAttr ((Background (r, g, b)) : as) = (rgbBg r g b) ++ escAttr as
escAttr (Reset : as) = reset ++ escAttr as
escAttr ((Custom s) : as) = s ++ escAttr as
