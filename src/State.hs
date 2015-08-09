module Mu.State (
    changeState
) where

import Mu.Commands
import Mu.Types
import Mu.Utils

changeState :: Editor -> IO Editor
changeState ed = return $ update (edState ed) (edInput ed) ed

update :: State -> Input -> Editor -> Editor
update _ None ed = ed

{-- State changes for Insert mode --}

-- Insert characters before cursor.
update Insert (Chars s) ed =
  insertText mainBuf s ed

-- Delete character before cursor.
update Insert Backspace ed =
  deleteText mainBuf (-1) ed 

-- Delete character under cursor.
update Insert Delete ed =
  deleteText mainBuf 0 ed 

-- Move cursor left.
update Insert LeftArr ed =
  moveCurs mainBuf (-1) ed

-- Move cursor right.
update Insert RightArr ed =
  moveCurs mainBuf 1 ed

-- Change to Select mode
update Insert Escape ed = setText commandBuf "" $ 
  ed { edState  = (Select Normal),
       edActive = commandBuf }

{-- State changes for Select mode --}

update (Select _) (Chars "\n") ed = ed' { edState = Insert,
                                          edActive = mainBuf }
  where ed' = case getBuffer commandBuf ed of
                Nothing -> ed
                Just b  ->
                  let t = bufText b
                      m = "Command not found: " ++ t
                   in case startCommand t ed of
                    Just e  -> e
                    Nothing -> setText commandBuf m ed

-- Insert characters before cursor
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

-- Write a debug message.
update _ i ed =
  change "command" ed $ \ b ->
    b { bufText = justLeft (fst $ bufSize b) 
                $ "Input not handled: " ++ show i }
