module Mu.Utils (
  termSize,
  addBuffer,
  getBuffer,
  changeBuffer,
  inter,
  ripOut,
  toTwo,
  cursTwo,
  clearScreen
) where

import Mu.Types
import Mu.Outlib

addBuffer :: String -> Buffer -> Editor -> Editor
addBuffer k b e = 
    e { edBuffers = (k, b) : edBuffers e } 

getBuffer :: String -> Editor -> Maybe Buffer
getBuffer k e = lookup k (edBuffers e)

exclude :: Eq a => a -> [(a,b)] -> [(a,b)]
exclude _ [] = []
exclude k (e:ts)
  | k == fst e = exclude k ts
  | otherwise  = e : exclude k ts

changeBuffer :: String -> Editor -> (Buffer -> Buffer) -> Editor
changeBuffer k e f = case getBuffer k e of
                      Nothing -> e
                      Just b  ->
                        e { edBuffers = (k, f b) : (exclude k $ edBuffers e) }

inter :: Int -> [a] -> [a] -> [a]
inter i es ls = take i ls ++ es ++ drop i ls

ripOut :: Int -> [a] -> [a]
ripOut i xs
  | i <  0 = xs
  | i >= l = xs
  | True   = (take i xs) ++ drop (i+1) xs
  where l = length xs

toTwo :: String -> Int -> Point
toTwo s i = (foldl (.) id $ reverse $ take i $ foldl (++) [] $ map ff s) (0, 0)
  where ff c = case c `elem` "\n\r" of
                 True  -> [\(_, y) -> (0, y+1)]
                 False -> [\(x, y) -> (x+1, y)]

cursTwo :: Buffer -> Point
cursTwo b = toTwo t c
  where o = bufOff b
        t = drop o $ bufText b
        c = bufCurs b - o
