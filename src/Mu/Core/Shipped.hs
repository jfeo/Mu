module Mu.Core.Shipped where

import Data.Default
import System.Exit
import Mu.Buffer
import Mu.Part
import Mu.Types
import Mu.Var
import Mu.Utils
import Mu.Core.Buffers
import Mu.Core.Outlib

shippedParts :: [Part]
shippedParts = [openPart, savePart, quitPart, aliasPart, konamiPart]

quitPart :: Part
quitPart = def { partName  = "quit"
               , partFun   = once quit
               }

konamiPart :: Part
konamiPart = def { partName  = "konami"
                 , partFun   = once konami
                 }

quit :: Part -> Mu -> IO Mu
quit _ mu = do
  clearScreen
  putStr reset
  _ <- exitSuccess
  return mu

openPart :: Part
openPart = def { partName   = "open"
               , partFun    = once open
               }

open :: Part -> Mu -> IO Mu
open this mu =
  let name = partName this in
  case getArgs name mu of
    []   -> return
          $ stopPart name
          $ changeBuffer commandBufName (setText "No filename given!") mu
    (f:_) -> do
        t <- readFile f
        return $ setVar "filename" f
               $ changeBuffer commandBufName
                   (setText $ "'" ++ f ++ "' successfully loaded")
               $ changeBuffer mainBufName (setText t)
               $ stopPart name mu

savePart :: Part
savePart = def { partName   = "save"
               , partMoment = 55
               , partFun    = once save
               }

save :: Part -> Mu -> IO Mu
save this mu =
  let name = partName this in
  case getVar "filename" mu of
    Just "" -> return 
             $ changeBuffer commandBufName (setText "Please enter a filename.") mu
    Just f  -> saveMainBuffer f mu
    Nothing ->
      case getArgs name mu of
        []    -> return 
               $ changeBuffer commandBufName (setText "Please enter a filename.") mu
        (f:_) -> saveMainBuffer f mu

saveMainBuffer :: String -> Mu -> IO Mu
saveMainBuffer f mu = do
  let t = option "" 
        $ withBuffer mainBufName mu bufText
  writeFile f t
  return $ changeBuffer commandBufName (setText $ "'" ++ f ++ "' saved.") mu

aliasPart :: Part
aliasPart = def { partName = "alias"
                , partFun  = once alias
                }

alias :: Part -> Mu -> IO Mu
alias this mu = do
  let name = partName this in
    case getArgs name mu of
      []      -> return mu
      (_:[])  -> return mu
      (a:n:_) -> return
        $ option mu 
        $ withPart n mu
          (\ p -> let p' = p { partName = a }
                   in mu { muParts = p' : muParts mu }
          )

konami:: Part -> Mu -> IO Mu
konami _ mu = return
            $ changeBuffer mainBufName 
              (\ b -> b { bufText = unlines 
                                  $ reverse 
                                  $ lines
                                  $ bufText b }
              )
            $ mu
