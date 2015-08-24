{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mu.Types where

import Data.Default

-- | Instances on which a lookup can be performed.
class Keyed k a | a -> k where
    from :: k -> [a] -> Maybe a
    prune :: k -> [a] -> [a]

type Point = (Int, Int)

--- Input type ---

data Input = Chars String
           | UpArr
           | DownArr
           | RightArr
           | LeftArr
           | SUpArr
           | SDownArr
           | SRightArr
           | SLeftArr
           | MUpArr
           | MDownArr
           | MRightArr
           | MLeftArr
           | Escape
           | Backspace
           | Delete
           | None
           | Otherinput String
             deriving (Show, Read, Eq)

--- Buffer and related types ---

-- | The buffer type witch contains all text visible to the user.
data Buffer = Buffer {
    bufName :: String,     -- ^ The name of this buffer, used for lookups.
    bufCurs :: Int,        -- ^ The cursor position (character index in 'bufText').
    bufOff  :: Int,        -- ^ The offset, characters to drop before drawing.
    bufText :: String,     -- ^ The text ...
    bufLook :: [TextLook], -- ^ Looks for the text (colors, emph. etc.).
    bufDefLook  :: [Look], -- ^ The default look of this buffer (eg. background color).
    bufPos  :: Point,      -- ^ The (x, y) position on screen.
    bufSize :: Point       -- ^ The (w, h) dimensions on screen.
} deriving (Show, Read)

-- The default buffer value, for convenience
instance Default Buffer where
    def = Buffer "" 0 0 "" [] [Reset] (0, 0) (0, 0)

-- Lookups can be performed on a list of buffers by buffer name.
instance Keyed String Buffer where
    prune name bufs = filter (\b -> bufName b /= name) bufs
    from name bufs =
      case filter (\ b -> bufName b == name) bufs of
        []    -> Nothing
        (b:_) -> Just b

-- | Look for a portion of a larger text.
data TextLook = TextLook { lookStart :: Int  -- ^ The character index to start the look on.
                         , lookEnd   :: Int    -- ^ The character index to end the look on.
                         , looks     :: [Look]   -- ^ The looks to be started/ended.
                         , lookTag   :: String -- ^ A tag for finding textlooks again later (when contained in a list).
                         } deriving (Show, Read)

-- | Terminal color type, currently only values of 0-5 (256 colors) is supported.
data Color = RGB { red :: Int
                 , green :: Int
                 , blue :: Int
                 }
           | C256 { colorIndex :: Int }
             deriving (Show, Read)

-- | Defines how text looks when printed on the screen.
data Look = Foreground Color
          | Background Color
          | Reset
          | Custom String
            deriving (Show, Read)

--- Part type ---

-- | A "part" of the Mu application, which provides some functionality
data Part = Part { partName    :: String      -- ^ The name of this part, used for invoking
                 , partInstant :: Bool        -- ^ If a part is instant, it have effect right away.
                 , partMoment  :: Int         -- ^ The moment in time when this part runs.
                 , partStarted :: Bool        -- ^ Is this part running?
                 , partArgs    :: [String]    -- ^ Arguments for this part.         
                 , partFun     :: Part -> Mu -> IO Mu -- ^ The function of this part, which makes changes to Mu.
                 }

-- Lookups can be performed with the part name.
instance Keyed String Part where
    from n parts =
      case filter (\c -> n == partName c) parts of
        []    -> Nothing
        (x:_) -> Just x
    
    prune n parts = filter (\c -> n /= partName c) parts

-- Default part value.
instance Default Part where
    def = Part "" False 51 False [] (\_ mu -> return mu)

-- This is useful for debugging since 'partFun' cannot be shown/read.
instance Show Part where
    show p = (partName p) ++ ":" ++ (show $ partMoment p)

--- Var type ---

-- | A variable for storing some value for later use.
data Var = Var { varName   :: String -- ^ The name of this variable.
               , varValue  :: String -- ^ The value of this variable.
               } deriving (Show, Read)

-- Lookups can be performed on variables by name.
instance Keyed String Var where
    from n vs =
      case filter (\ v -> n == varName v) vs of
        []    -> Nothing
        (v:_) -> Just v
    prune n vs = filter (\ v -> n /= varName v) vs

--- Mu type ---

-- | The main Mu type, where all the magic happens.
data Mu = Mu { muMode    :: Mode     -- ^ The current mode.
             , muInput   :: Input    -- ^ The last user input.
             , muActive  :: String   -- ^ The name of the active buffer.
             , muBuffers :: [Buffer] -- ^ All the buffers
             , muVars    :: [Var]    -- ^ All the variables.
             , muParts   :: [Part]   -- ^ All the parts.
             } deriving (Show)

setMode :: Mode -> Mu -> Mu
setMode m mu = mu { muMode = m }

setActive :: String -> Mu -> Mu
setActive s mu = mu { muActive = s }

instance Default Mu where
    def = Mu Insert None "" [] [] []

-- | The different editor modes.
data Mode = Insert
          | Select SelMode
          | OtherMode String
          deriving (Show, Read)

-- | Selection mode.
data SelMode = Normal
             | Line
             | Block
               deriving (Show, Read)