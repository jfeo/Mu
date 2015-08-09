module Mu.Display (
  display
) where

import Mu.Types
import Mu.Utils
import Mu.Outlib

-- | Displays the editor.
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

displayText :: Int -> Int -> Int -> 
               String -> [TextAttr] -> [Attr] ->
               Int -> Int -> Int -> String
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

escAttr :: [Attr] -> String
escAttr [] = ""
escAttr ((Foreground (r, g, b)) : as) = (rgbFg r g b) ++ escAttr as
escAttr ((Background (r, g, b)) : as) = (rgbBg r g b) ++ escAttr as
escAttr (Reset : as) = reset ++ escAttr as
escAttr ((Custom s) : as) = s ++ escAttr as
