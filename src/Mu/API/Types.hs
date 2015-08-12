{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mu.API.Types where

import Data.Default

-- | Instances on which a lookup can be performed.
class Look k a | a -> k where
    look :: k -> [a] -> Maybe a
    prune :: k -> [a] -> [a]

type Point = (Int, Int)

-- Input type --

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
             deriving (Show, Read, Eq)

-- Buffer and related types --

data Buffer = Buffer {
    bufName :: String,
    bufCurs :: Int,
    bufOff  :: Int,
    bufText :: String,
    bufAttr :: [TextAttr],
    bufDefAttr  :: [Attr],
    bufPos  :: Point,
    bufSize :: Point
} deriving (Show, Read)

instance Default Buffer where
    def = Buffer "" 0 0 "" [] [Reset] (0, 0) (0, 0)

instance Look String Buffer where
    prune name bufs = filter (\b -> bufName b /= name) bufs
    look name bufs =
      case filter (\ b -> bufName b == name) bufs of
        []    -> Nothing
        (b:_) -> Just b

type TextAttr = (Int, Int, [Attr])

type Color = (Int, Int, Int)

data Attr = Foreground Color
          | Background Color
          | Reset
          | Custom String
            deriving (Show, Read)


-- Command type --

data Command = Command {
    cmdName    :: String,
    cmdLevel   :: Int,
    cmdStarted :: Bool,
    cmdParse   :: String -> Maybe [String],
    cmdArgs    :: [String],
    cmdFun     :: Editor -> IO Editor
}

instance Look String Command where
    look n cmds =
      case filter (\c -> n == cmdName c) cmds of
        []    -> Nothing
        (x:_) -> Just x
    
    prune n cmds = filter (\c -> n /= cmdName c) cmds

type CommandDef = (String, Command)

instance Default Command where
    -- level 14 is between resize and display
    def = Command "" 14 False (\_ -> Nothing) [] return

instance Show Command where
    show p = (cmdName p) ++ ":" ++ (show $ cmdLevel p)

-- Editor type --

data Editor = Editor {
    edState    :: State,
    edInput    :: Input,
    edActive   :: String,
    edBuffers  :: [Buffer],
    edVars     :: [(String, String)],
    edCommands :: [Command]
} deriving (Show)

instance Default Editor where
    def = Editor Insert None "" [] [] []

data State = Insert
           | Select SelType
           | Other String
             deriving (Show, Read)

data SelType = Normal
             | Line
             | Block
               deriving (Show, Read)

-- Changeable typeclass and instances --

class Eq k => Changeable k a b where
  change :: k -> b -> (a -> a) -> b

instance Changeable String Buffer Editor where
  change k e f = 
    case k `look` edBuffers e of
      Nothing -> e
      Just b  -> e { edBuffers = f b : (prune k $ edBuffers e) }

instance Changeable String Command Editor where
  change k e f = 
    case k `look` edCommands e of
      Nothing -> e
      Just c  -> e { edCommands = (f c) : (prune k $ edCommands e) }
