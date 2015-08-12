{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}

module Mu.API.Utils (
  addBuffer,
  getBuffer,
  changeBuffer,
  inter,
  exclude,
  ripOut,
  toTwo,
  cursTwo,
  clearScreen,
  justCenter,
  justLeft,
  insertText,
  deleteText,
  moveCurs,
  change,
  getVar,
  setVar,
  showVar,
  readVar,
  setSize,
  setPos,
  setText,
  cursX,
  cursY,
  lineIndex
) where

import Mu.API.Types
import Mu.Core.Outlib

addBuffer :: Buffer -> Editor -> Editor
addBuffer b e = 
    e { edBuffers = b : (prune (bufName b) $ edBuffers e) } 

getBuffer :: String -> Editor -> Maybe Buffer
getBuffer k e = look k $ edBuffers e

exclude :: Eq a => a -> [(a,b)] -> [(a,b)]
exclude _ [] = []
exclude k (e:ts)
  | k == fst e = exclude k ts
  | otherwise  = e : exclude k ts

changeBuffer :: String -> Editor -> (Buffer -> Buffer) -> Editor
changeBuffer k e f = case getBuffer k e of
                      Nothing -> e
                      Just b  ->
                        e { edBuffers = f b : (prune k $ edBuffers e) }

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

justCenter :: Int -> String -> String
justCenter n s
  | n <= 0    = s
  | n <= l    = s
  | odd l     = justCenter n $ s ++ " "
  | otherwise = justCenter n $ " " ++ s
    where l = length s

justLeft :: Int -> String -> String
justLeft n s
  | n <= 0    = s
  | n <= l    = s
  | otherwise = justLeft n $ s ++ " "
    where l = length s


insertText :: String -> String -> Editor -> Editor
insertText n s ed = 
  change n ed $ \ b ->
    b { bufText = inter (bufCurs b) s (bufText b),
        bufCurs = length s + bufCurs b }

deleteText :: String -> Int -> Editor -> Editor
deleteText n i ed = 
  change n ed $ \ b ->
    let c = bufCurs b
        l = length $ bufText b
     in b { bufText = ripOut (c + i) $ bufText b,
            bufCurs = min l $ max 0 (c + i) }

moveCurs :: String -> Int -> Editor -> Editor
moveCurs n i ed = 
  change n ed $ \ b ->
    let c = bufCurs b
        l = length $ bufText b
     in b { bufCurs = min l $ max 0 (c + i) }

getVar :: String -> Editor -> Maybe String
getVar k ed = k `lookup` edVars ed

setVar :: String -> String -> Editor -> Editor 
setVar k v ed =
  let vars = edVars ed
   in ed { edVars = (k,v) : exclude k vars }

showVar :: Show a => String -> a -> Editor -> Editor
showVar k v ed = setVar k (show v) ed

readVar :: Read a => String -> Editor -> Either String a
readVar k ed =
  case getVar k ed of
    Nothing -> Left "Variable not found"
    Just s  ->
      case reads s of
       []    -> Left $ "Could not parse: " ++ s
       (v:_) -> Right $ fst v

setSize :: String -> Int -> Int -> Editor -> Editor
setSize n w h ed =
  change n ed $ \ b ->
    b { bufSize = (w, h) }

setPos :: String -> Int -> Int -> Editor -> Editor
setPos n x y ed =
  change n ed $ \ b ->
    b { bufPos = (x, y) }

setText :: String -> String -> Editor -> Editor
setText n s ed =
  change n ed $ \ b ->
    b { bufText = s }

cursX :: Int -> String -> Int
cursX c s = 
  case filter (\ i -> i <= c) 
     $ scanl (+) 0 
     $ map length 
     $ map ((:)' ')
     $ lines s of
    [] -> c
    xs -> c - last xs

cursY :: Int -> String -> Int
cursY c s = length
          $ drop 1
          $ filter (\ i -> i <= c)
          $ scanl (+) 0
          $ map length 
          $ map ((:)' ')
          $ lines s

lineIndex :: Int -> String -> Int
lineIndex n s = if n >= length cs
                then (length cs) - 1
                else cs !! max 0 n
  where
    cs = scanl (+) 0
       $ map length 
       $ map ((:)' ')
       $ lines s

