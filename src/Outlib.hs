module Mu.Outlib(
    clearScreen,
    curSet,
    delLine,
    putUtfStr,
    termSize,
    reset,
    rgbFg,
    rgbBg,
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8  as U
import qualified System.Console.Terminal.Size as Term

reset :: String
reset = csi ++ "0m"

rgbFg :: Int -> Int -> Int -> String
rgbFg r g b = csi ++ "38;5;" ++ (show n) ++ "m" 
  where n = 16 + 36 * r + 6 * g + b

rgbBg :: Int -> Int -> Int -> String
rgbBg r g b = csi ++ "48;5;" ++ (show n) ++ "m" 
  where n = 16 + 36 * r + 6 * g + b

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

termSize :: IO (Int, Int)
termSize = do
    w <- termw
    h <- termh
    return (w, h)

curSet :: Int -> Int -> String
curSet x y = csi ++ (show $ y+1) ++ ";" ++ (show $ x+1) ++ "H"

clearScreen :: IO ()
clearScreen = do
    putUtfStr $ curSet 0 0
    h <- termh
    let ls = [0 .. h]
    putUtfStr $ concat $ map (\i -> (curSet 0 i) ++ delLine) ls
    putUtfStr $ curSet 0 0

putUtfStr :: String -> IO ()
putUtfStr s = do
    C.putStr $ U.fromString s

csi :: String
csi = "\ESC["

delLine :: String
delLine = csi ++ "2K"
