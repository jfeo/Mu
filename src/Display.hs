module Mu.Display (
  display
) where

import Mu.Types
import Mu.Utils
import Mu.Outlib
import Data.List (sortBy, elemIndex)

-- | Displays the editor.
display :: Editor -> IO ()
display ed = do
  clearScreen
  let bufs = edBuffers ed
  sequence_ $ map (\(n, b) -> displayBuffer b) bufs
  case getBuffer "main" ed of
    Nothing -> putStrLn "Not main buffer!"
    Just b  -> do
       let (k, r) = cursTwo b
           (x, y) = bufPos b
           (u, v) = (k+x, r+y)
       curSet v u
       putUtfStr "\0"

escAttr :: [Attr] -> String
escAttr [] = ""
escAttr ((Foreground (r, g, b)):as) = (rgbFg r g b) ++ escAttr as
escAttr ((Background (r, g, b)):as) = (rgbBg r g b) ++ escAttr as
escAttr (Reset:as) = reset ++ escAttr as

lineLabel :: Int -> (String, [TextAttr]) -> [Label]
lineLabel y (s, as) =
    let f (p,l,a) = Label p y l (take l $ drop p s) (escAttr a) in
        map f as

index xs = zip [0..length xs - 1] xs

attrLines :: Int -> String -> [TextAttr] -> [(String, [TextAttr])]
attrLines i "" _ = []
attrLines i s [] = [(s, [(i, length s - i, [])])]
attrLines i s ((p,n,a):as) =
    let b  = i == p
        p' = if b then p else i
        n' = if b then n else (p-i)
        a' = if b then a else []
        s' = take n' s
        l  = length s
    in (s', [(p',n',a')]) : attrLines (i+l) (drop n' s) as

-- | Displays a single buffer.
displayBuffer :: Buffer -> IO ()
displayBuffer b = do
    let o  = bufOff  b
        c  = bufCurs b
        (x, y) = bufPos b
        (w, h) = bufSize b
        t  = drop o $ bufText b
    return ()
