-- | Provides one command 'changeStateCmd', which is responsible
--   for change to the editor state, as well as mapping user input
--   to text editing functions.
module Mu.Core.State ( changeStateCmd
                     , changeState
                     ) where

import Data.Default
import Mu.Core.Buffers
import Mu.API.Buffer
import Mu.API.Command
import Mu.API.Types
import Mu.API.Utils

-- | The /core/ command which changes the state (in a
--   broad sense) of the editor, based on the edInput
--   field.
--   Default command level is 8.
changeStateCmd :: Command
changeStateCmd = def { cmdName    = "core_state"
                     , cmdLevel   = 8
                     , cmdFun     = changeState
                     , cmdStarted = True
                     }

-- | Wrapper function for 'update'.
changeState :: Editor -> IO Editor
changeState ed = return $ update (edState ed) (edInput ed) ed

-- | Updates the state of the editor by pattern matching.
--   The 'State' and 'Input' values should also match those
--   of the Editor value.
update :: State -> Input -> Editor -> Editor
update _ None ed = ed

{-- State changes for Insert mode --}

update Insert (Chars s) ed =
  insertText mainBufName s ed

update Insert Backspace ed =
  deleteText mainBufName (-1) ed 

update Insert Delete ed =
  deleteText mainBufName 0 ed 

update Insert LeftArr ed =
  moveCurs mainBufName (-1) ed
  
update Insert RightArr ed =
  moveCurs mainBufName 1 ed

update Insert UpArr ed =
  change mainBufName ed $ \b ->
    let c = bufCurs b
        t = bufText b
        cY = cursY c t
        cX = cursX c t
        li = lineIndex (cY-1) t
        ll = length $ (lines t) !! (cY-1)
        c' = li + (min ll cX)
     in if cY == 0 then b
                   else fixOffset $ b { bufCurs = c' }

update Insert DownArr ed =
  change mainBufName ed $ \b ->
    let c = bufCurs b
        t = bufText b
        cY = cursY c t
        mY = length $ lines t
        cX = cursX c t
        li = lineIndex (cY+1) t
        ll = length $ (lines t) !! (cY+1)
        c' = li + (min ll cX)
     in if (cY+1) >= mY then b
                        else fixOffset $ b { bufCurs = c' }

update Insert Escape ed = setText commandBufName "" $ 
  ed { edState  = (Select Normal),
       edActive = commandBufName }

{-- State changes for Select mode --}

update (Select _) (Chars "\n") ed = ed' { edState  = Insert
                                        , edActive = mainBufName
                                        }
  where 
    ed' = maybe ed id 
        $ withBuffer commandBufName ed $ \ b ->
            startCommands (bufText b) ed 

update (Select _) (Chars s) ed =
  insertText commandBufName s ed

update (Select _) Backspace ed =
  deleteText commandBufName (-1) ed

update (Select _) Delete ed =
  deleteText commandBufName 0 ed

update (Select _) LeftArr ed =
  moveCurs commandBufName (-1) ed

update (Select _) RightArr ed =
  moveCurs commandBufName 1 ed

update (Select _) Escape ed =
  ed { edState  = Insert,
       edActive = mainBufName }

-- For now, fail if the input is unmatched.
update s i _ = error $ "No state changing: " ++ (show s) ++ " " ++ (show i)

fixOffset :: Buffer -> Buffer
fixOffset b
  | diff >= h   = fixOffset
                $ b { bufOff = min ln $ lineIndex (offY + 1) text }
  | curY < offY = fixOffset
                $ b { bufOff = max 0 $ lineIndex  (offY - 1) text }
  | otherwise   = b
  where
    text = bufText b
    ln   = length text
    off  = bufOff b
    offY = cursY off text
    cur  = bufCurs b
    curY = cursY cur text
    diff = curY - offY
    (_,h) = bufSize b

