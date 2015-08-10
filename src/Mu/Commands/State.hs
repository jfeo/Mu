-- | Provides one command 'changeStateCmd', which is responsible
--   for change to the editor state, as well as mapping user input
--   to text editing functions.
module Mu.Commands.State ( changeStateCmd
                         , changeState
                         ) where

import Data.Default
import Mu.Commands
import Mu.Types
import Mu.Utils

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
  insertText mainBuf s ed

update Insert Backspace ed =
  deleteText mainBuf (-1) ed 

update Insert Delete ed =
  deleteText mainBuf 0 ed 

update Insert LeftArr ed =
  moveCurs mainBuf (-1) ed
  
update Insert RightArr ed =
  moveCurs mainBuf 1 ed

update Insert Escape ed = setText commandBuf "" $ 
  ed { edState  = (Select Normal),
       edActive = commandBuf }

{-- State changes for Select mode --}

update (Select _) (Chars "\n") ed = 
    ed' { edState = Insert,
        edActive = mainBuf }
  where 
    ed' = case getBuffer commandBuf ed of
            Nothing -> ed
            Just b  ->
              let t = bufText b
               in startCommand t ed

update (Select _) (Chars s) ed =
  insertText commandBuf s ed

update (Select _) Backspace ed =
  deleteText commandBuf (-1) ed

update (Select _) Delete ed =
  deleteText commandBuf 0 ed

update (Select _) LeftArr ed =
  moveCurs commandBuf (-1) ed

update (Select _) RightArr ed =
  moveCurs commandBuf 1 ed

update (Select _) Escape ed =
  ed { edState  = Insert,
       edActive = mainBuf }

-- For now, fail if the input is unmatched.
-- update s i _ = error $ "No state changing: " ++ (show s) ++ " " ++ (show i)
