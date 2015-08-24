module Mu.Buffer ( changeBuffer
                 , withBuffer
                 , addBuffer
                 , removeBuffer
                 , getBuffer
                 , setCurs
                 , setOff
                 , setText
                 , setLook
                 , setDefLook
                 , setPos
                 , setSize
                 , indexXY
                 , moveCurs
                 , insertText
                 , deleteText
                 , fixOffset
                 ) where

import Mu.Types
import Mu.Utils

changeBuffer :: String -> (Buffer -> Buffer) -> Mu -> Mu
changeBuffer k f mu = 
    case k `from` muBuffers mu of
      Nothing -> mu
      Just b  -> mu { muBuffers = f b : (prune k $ muBuffers mu) }

withBuffer :: String -> Mu -> (Buffer -> a) -> Maybe a
withBuffer k mu f = 
    case k `from` muBuffers mu of
      Nothing -> Nothing
      Just b  -> Just $ f b 

-- | Add a buffer.
addBuffer :: Buffer -> Mu -> Mu
addBuffer b mu = 
    mu { muBuffers = b : (prune (bufName b) $ muBuffers mu) } 

removeBuffer :: Buffer -> Mu -> Mu
removeBuffer b mu =
    mu { muBuffers = prune (bufName b) $ muBuffers mu}

-- | Get a buffer by it's name.
getBuffer :: String -> Mu -> Maybe Buffer
getBuffer k mu = k `from` muBuffers mu

-- | Sets the cursor position.
setCurs :: Int -> Buffer -> Buffer
setCurs c b = b { bufCurs = c }

-- | Sets the offset position.
setOff :: Int -> Buffer -> Buffer
setOff c b = b { bufOff = c }

-- | Sets the text.
setText :: String -> Buffer -> Buffer
setText s b = b { bufText = s }

-- | Sets the text look
setLook :: [TextLook] -> Buffer -> Buffer
setLook ls b = b { bufLook = ls }

-- | Sets the default look.
setDefLook :: [Look] -> Buffer -> Buffer
setDefLook ls b = b { bufDefLook = ls }

-- | Sets the position of a buffer, by name.
setPos ::  Int -> Int -> Buffer -> Buffer
setPos x y b = b { bufPos = (x, y) }

-- | Sets the size of a buffer, by name.
setSize :: Int -> Int -> Buffer -> Buffer
setSize w h b = b { bufSize = (w, h) }

indexXY :: Buffer -> (Buffer -> Int) -> Point
indexXY b f = (indexInLine c t, lineIndex c t)
 where c = f b
       t = bufText b

-- | Move the cursor of a buffer, by name, bounded by
--   @
--      [0, length $ bufText b]
--   @
moveCurs :: Int -> Buffer -> Buffer
moveCurs i b = fixOffset
             $ b { bufCurs = min l $ max 0 (c + i) }
  where c = bufCurs b
        l = length $ bufText b

-- Insert string at buffer cursor position.
insertText :: String -> Buffer -> Buffer
insertText s b = fixOffset
               $ moveCurs (length s)
               $ b { bufText = splice (bufCurs b) s (bufText b) }

-- | Delete character under cursor from buffer text.
deleteText :: Buffer -> Buffer
deleteText b = fixOffset
             $ b { bufText = remove (bufCurs b) $ bufText b }

fixOffset :: Buffer -> Buffer
fixOffset b
  | diff > h-1  = fixOffset
                $ b { bufOff = min ln $ indexOfLine (offY + 1) text }
  | curY < offY = fixOffset
                $ b { bufOff = max 0 $ indexOfLine  (offY - 1) text }
  | otherwise   = b
  where
    text = bufText b
    ln   = length text
    (_, offY) = indexXY b bufOff
    (_, curY) = indexXY b bufCurs
    diff  = (curY - offY)
    (_,h) = bufSize b
