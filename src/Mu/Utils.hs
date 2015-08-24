module Mu.Utils where

-- | Insert a list into another list at index.
splice :: Int -> [a] -> [a] -> [a]
splice i es ls = take i ls ++ es ++ drop i ls

-- | Remove an elementV
remove :: Int -> [a] -> [a]
remove i xs
  | i <  0 = xs
  | i >= l = xs
  | True   = (take i xs) ++ drop (i+1) xs
  where l = length xs

-- | The distance from the nearest linebreak to
--   the given index (The x position of the index).
indexInLine :: Int -> String -> Int
indexInLine c s = 
  case filter (\ i -> i <= c) 
     $ scanl (+) 0 
     $ map length 
     $ map ((:)' ')
     $ lines s of
    [] -> 0
    xs -> c - last xs

-- | The index of the line on which
--   the given index lies (The y position of the index).
lineIndex :: Int -> String -> Int
lineIndex c s = length
              $ drop 1
              $ filter (\ i -> i <= c)
              $ scanl (+) 0
              $ map length 
              $ map ((:)' ')
              $ lines s

-- | The character index where the line
--   given index starts.
indexOfLine :: Int -> String -> Int
indexOfLine n s = if n >= length cs
                  then (length cs)
                  else cs !! max 0 n
  where
    cs = scanl (+) 0
       $ map length 
       $ map ((:)' ')
       $ lines s

-- | Fit text en center of a new string
--   with width 'w'.
justCenter :: Int -> String -> String
justCenter w s
  | w <= 0    = s
  | w <= l    = s
  | odd l     = justCenter w $ s ++ " "
  | otherwise = justCenter w $ " " ++ s
    where l = length s

-- | Pads the string to the right, until
--   string length is 'w'.
justLeft :: Int -> String -> String
justLeft w s
  | w <= 0    = s
  | w <= l    = s
  | otherwise = justLeft w $ s ++ " "
    where l = length s

carryIO :: (a -> b -> IO a) -> a -> [b] -> IO a
carryIO _ a []     = return a
carryIO f a (b:bs) = do
  a' <- f a b
  carryIO f a' bs

option :: a -> Maybe a -> a
option x Nothing  = x
option _ (Just y) = y