module Mu.Outlib(
    Drawable (..),
    Label (..),
    clearScreen,
    curSet,
    delLine,
    termh,
    termw,
    putUtfStr,
    reset,
    rgbFg,
    rgbBg,
    testDraw
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8  as U
import qualified System.Console.Terminal.Size as Term

class Drawable a where
    objectW  :: a -> Int
    objectH :: a -> Int
    objectX :: a -> Int
    objectY :: a -> Int 
    draw :: a -> IO ()
    objectR :: a -> Int
    objectR o = (objectX o) + objectW o
    objectB :: a -> Int
    objectB o = (objectY o) + objectH o

data Label = Label {
    labelX :: Int,
    labelY :: Int,
    labelW :: Int,
    labelText :: String,
    labelHeader :: String
} deriving (Show, Eq)

instance Drawable Label where
    objectW  l = labelW l
    objectH l = 1
    objectX l = labelX l
    objectY l = labelY l
    draw l = do
        let h = labelHeader l
            s = take (objectW l) $ labelText l
            p = replicate (objectW l - length s) ' '
        curSet (objectY l) (objectX l)
        putUtfStr $ h ++ s ++ p ++ csi ++ "0m"

reset :: String
reset = csi ++ "0m"

rgbFg :: Int -> Int -> Int -> String
rgbFg r g b = csi ++ "38;5;" ++ (show n) ++ "m" 
  where n = 16 + 36 * r + 6 * g + b

rgbBg :: Int -> Int -> Int -> String
rgbBg r g b = csi ++ "48;5;" ++ (show n) ++ "m" 
  where n = 16 + 36 * r + 6 * g + b

testDraw :: Drawable a => a -> IO ()
testDraw o = do
    clearScreen
    draw o
    getChar
    clearScreen

termw :: IO Int
termw = do
    s <- Term.size
    case s of
      Nothing -> return 0
      Just w  -> return $ Term.width w

termh :: IO Int
termh = do
    s <- Term.size
    case s of
      Nothing -> return 0
      Just w  -> return $ Term.height w

clearScreen :: IO ()
clearScreen = do
    curSet 0 0
    h <- termh
    let ls = [0 .. h]
    sequence_ $ map (\i -> curSet i 0 >> delLine) ls
    curSet 0 0

putUtfStr :: String -> IO ()
putUtfStr s = do
    C.putStr $ U.fromString s

toCode :: Enum a => Int -> a -> String
toCode b c = show $ b + fromEnum c

csi :: String
csi = "\ESC["

curSet :: Int -> Int -> IO ()
curSet r c = do
    putStr $ csi ++ (show $ r+1) ++ ";" ++ (show $ c+1) ++ "H"

curCol :: Int -> IO ()
curCol c = putStr $ csi ++ (show $ c+1) ++ "G"

curBol :: IO ()
curBol = putStr $ csi ++ "0G"

curUp    n = putStr $ csi ++ (show n) ++ "A"
curDown  n = putStr $ csi ++ (show n) ++ "B"
curRight n = putStr $ csi ++ (show n) ++ "C"
curLeft  n = putStr $ csi ++ (show n) ++ "D"

delLine :: IO ()
delLine = do
    putStr $ csi ++ "2K"
