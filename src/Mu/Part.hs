module Mu.Part ( changePart
               , withPart
               , setMoment
               , setStarted
               , setArgs
               , getArgs
               , startPart
               , stopPart
               , runPart
               , runPartsByName
               , runInOrder
               , once
               ) where

import Mu.Types
import Mu.Utils
import Data.List (nub, sort)

changePart :: String -> (Part -> Part) -> Mu -> Mu
changePart k f mu = 
  case k `from` muParts mu of
    Nothing -> mu
    Just c  -> mu { muParts = (f c) : (prune k $ muParts mu) }

withPart :: String -> Mu -> (Part -> a) -> Maybe a
withPart k mu f =
  case k `from` muParts mu of
    Nothing -> Nothing
    Just p  -> Just $ f p

setMoment :: Int -> Part -> Part
setMoment m p = p { partMoment = m }

setStarted :: Bool -> Part -> Part
setStarted b p = p { partStarted = b }

setArgs :: [String] -> Part -> Part
setArgs as p = p { partArgs = as }

getArgs :: String -> Mu -> [String]
getArgs name mu = maybe [] id
                $ withPart name mu partArgs

startPart :: String -> Mu -> Mu
startPart name mu = 
  changePart name (setStarted True) mu

stopPart :: String -> Mu -> Mu
stopPart name mu = 
  changePart name (setStarted False) mu

runPartsByName :: [String] -> Mu -> IO Mu
runPartsByName []     mu = return mu
runPartsByName (n:ns) mu = do
  mu' <- option (return mu)
       $ withPart n mu (\p -> runPart p mu)
  runPartsByName ns mu'

runPart :: Part -> Mu -> IO Mu
runPart p mu = (partFun p) p mu

runInOrder :: Mu -> IO Mu
runInOrder mu = _runInOrder (sort $ nub $ map partMoment $ muParts mu) mu

_runInOrder :: [Int] -> Mu -> IO Mu
_runInOrder [] mu     = return mu
_runInOrder (l:_) mu = do
  let parts = filter (\p -> l == partMoment p && partStarted p)
            $ muParts mu

  mu' <- carryIO (\m p -> runPart p m) mu parts

  let ls'   = filter (\ m -> m > l) 
            $ sort $ nub 
            $ map partMoment 
            $ muParts mu'

  _runInOrder ls' mu'

once :: (Part -> Mu -> IO Mu) -> (Part -> Mu -> IO Mu)
once f = \p mu -> do
  mu' <- f p mu
  return $ stopPart (partName p) mu'