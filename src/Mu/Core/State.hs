-- | Provides one command 'changeModeCmd', which is responsible
--   for change to the muitor state, as well as mapping user input
--   to text muiting functions.
module Mu.Core.State ( changeStatePart
                     , changeState
                     ) where

import Data.Default
import Mu.Core.Buffers
import Mu.Buffer
import Mu.Parser
import Mu.Part
import Mu.Types
import Mu.Utils

-- | The /core/ command which changes the state (in a
--   broad sense) of the muitor, based on the muInput
--   field.
--   Default command level is 8.
changeStatePart :: Part
changeStatePart = def { partName    = "core_state"
                      , partMoment  = 50
                      , partFun     = changeState
                      , partStarted = True
                      }

-- | Wrapper function for 'update'.
changeState :: Part -> Mu -> IO Mu
changeState _ mu = return $ update (muMode mu) (muInput mu) mu

-- | Updates the state of the muitor by pattern matching.
--   The 'Mode' and 'Input' values should also match those
--   of the Mu value.
update :: Mode -> Input -> Mu -> Mu
update _ None mu = mu

{-- Mode changes for Insert mode --}

update Insert (Chars s) mu =
  changeBuffer mainBufName (insertText s) mu

update Insert Backspace mu =
  changeBuffer mainBufName (deleteText . moveCurs (-1)) mu 

update Insert Delete mu =
  changeBuffer mainBufName deleteText mu 

update Insert LeftArr mu =
  changeBuffer mainBufName (moveCurs (-1)) mu
  
update Insert SLeftArr mu =
  changeBuffer mainBufName
    (\ b -> let t = bufText b
                (_, cY)
                  = indexXY b bufCurs
                c = indexOfLine cY t
             in fixOffset
              $ b { bufCurs = c }
    ) mu

update Insert RightArr mu =
  changeBuffer mainBufName (moveCurs 1) mu

update Insert SRightArr mu =
  changeBuffer mainBufName 
    (\ b -> let t = bufText b
                l = (length $ lines t) - 1
                (_,cY)
                  = indexXY b bufCurs
                c = indexOfLine (min (cY + 1) l) t
              in fixOffset
               $ b { bufCurs = max 0 $ c - 1}
    ) mu

update Insert UpArr mu =
  changeBuffer mainBufName 
    (\ b -> let t  = bufText b
                (cX, cY)
                   = indexXY b bufCurs
                li = indexOfLine (cY - 1) t
                ll = length $ (lines t) !! (cY - 1)
                c' = li + min ll cX
            in if cY <= 0 then b
                          else fixOffset $ b { bufCurs = c' }
    ) mu

update Insert DownArr mu =
  changeBuffer mainBufName 
    (\ b -> let t  = bufText b
                (cX, cY)
                   = indexXY b bufCurs
                mY = (length $ lines t) - 1
                li = indexOfLine (cY + 1) t
                ll = length $ (lines t) !! (cY + 1)
                c' = li + (min ll cX)
            in if cY == mY then b
                           else fixOffset $ b { bufCurs = c' }
    ) mu

update Insert Escape mu = changeBuffer commandBufName (setCurs 0 . setText "") 
                        $ mu { muMode  = (Select Normal)
                             , muActive = commandBufName 
                             }

{-- Mode changes for Select mode --}

update (Select _) (Chars "\n") mu = 
  case parseArguments t of
    Nothing     -> mu
    Just []     -> mu
    Just (n:as) -> setActive mainBufName
                 $ setMode Insert
                 $ changePart n (setArgs as . setStarted True)
                 $ mu
  where
    t   = option "" 
        $ withBuffer commandBufName mu bufText

update (Select _) (Chars s) mu =
  changeBuffer commandBufName (insertText s) mu

update (Select _) Backspace mu =
  changeBuffer commandBufName (deleteText . moveCurs (-1)) mu

update (Select _) Delete mu =
  changeBuffer commandBufName deleteText mu

update (Select _) LeftArr mu =
  changeBuffer commandBufName (moveCurs (-1)) mu

update (Select _) RightArr mu =
  changeBuffer commandBufName (moveCurs  1) mu

update (Select _) Escape mu =
  mu { muMode   = Insert
     , muActive = mainBufName
     }

-- For now, fail if the input is unmatched.
update _ _ mu = mu

