module Mu.Utils (
  termSize,
  addBuffer,
  getBuffer,
  changeBuffer,
  inter,
  toTwo,
  cursTwo,
  clearScreen
) where

import Mu.Types
import Mu.Outlib
import qualified System.Console.Terminal.Size as Term

termSize :: IO Point
termSize = do
    w <- termw
    h <- termh
    return (w, h)

addBuffer :: String -> Buffer -> Editor -> Editor
addBuffer k b e = 
    e { edBuffers = (k, b) : edBuffers e } 

getBuffer :: String -> Editor -> Maybe Buffer
getBuffer k e = lookup k (edBuffers e)

changeBuffer :: String -> Editor -> (Buffer -> Buffer) -> Editor
changeBuffer k e f = case getBuffer k e of
                      Nothing -> e
                      Just b  ->
                        e { edBuffers = (k, f b) : (edBuffers e) }

inter :: Int -> [a] -> [a] -> [a]
inter i es ls = take i ls ++ es ++ drop i ls

toTwo :: String -> Int -> Point
toTwo s i = (foldl (.) id $ reverse $ take i $ foldl (++) [] $ map ff s) (0, 0)
  where ff c = case c `elem` "\n\r" of
                 True  -> [\(x, y) -> (0, y+1)]
                 False -> [\(x, y) -> (x+1, y)]

cursTwo :: Buffer -> Point
cursTwo b = toTwo t c
  where o = bufOff b
        t = drop o $ bufText b
        c = bufCurs b - o
