module Mu.Types where

import Data.Default

type Point = (Int, Int)

type Color = (Int, Int, Int)

data Attr = Foreground Color
          | Background Color
          | Reset
          | Custom String
            deriving (Show, Read)

-- (Position, Length, Attribute)
type TextAttr = (Int, Int, [Attr])

data Buffer = Buffer {
    bufCurs :: Int,
    bufOff  :: Int,
    bufText :: String,
    bufAttr :: [TextAttr],
    bufDefAttr  :: [Attr],
    bufPos  :: Point,
    bufSize :: Point
} deriving (Show, Read)

instance Default Buffer where
    def = Buffer 0 0 "" [] [Reset] (0, 0) (0, 0)

data SelType = Normal
             | Line
             | Block
               deriving (Show, Read)

data State = Insert
           | Select SelType
           | Other String
             deriving (Show, Read)

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

data Command = Command {
    cmdLevel   :: Int,
    cmdStarted :: Bool,
    cmdParse   :: String -> Maybe [String],
    cmdArgs    :: [String],
    cmdFun     :: Editor -> IO Editor
}

instance Default Command where
    -- level 14 is between resize and display
    def = Command 14 False (\_ -> Nothing) [] return

instance Show Command where
    show p = show $ cmdLevel p

-- The head of edBufs is the active buffer.
data Editor = Editor {
    edState    :: State,
    edInput    :: Input,
    edActive   :: String,
    edBuffers  :: [(String, Buffer)],
    edVars     :: [(String, String)],
    edCommands :: [(String, Command)]
} deriving (Show)

instance Default Editor where
    def = Editor Insert None "" [] [] []

mainBuf :: String
mainBuf = "main"

statusBuf :: String
statusBuf = "status"

commandBuf :: String
commandBuf = "command"
