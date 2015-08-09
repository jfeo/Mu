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
    bufPos  :: Point,
    bufSize :: Point
} deriving (Show, Read)

instance Default Buffer where
    def = Buffer 0 0 "" [] (0, 0) (0, 0)

data SelType = Normal
             | Line
             | Block
               deriving (Show, Read)

data State = Insert
           | Select SelType
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
           | Delete
           | None
             deriving (Show, Read, Eq)

data Level = First
           | BeforeInput
           | BeforeState
           | BeforeDisplay
           | Last
           deriving (Show, Read)

data PlugIn = PlugIn {
    plugLevel :: Level,
    plugFun   :: Editor -> IO Editor
}

instance Show PlugIn where
    show p = show $ plugLevel p

-- The head of edBufs is the active buffer.
data Editor = Editor {
    edState   :: State,
    edInput   :: Input,
    edActive  :: String,
    edBuffers :: [(String, Buffer)],
    edVars    :: [(String, String)],
    edPlugIns :: [PlugIn]
} deriving (Show)

instance Default Editor where
    def = Editor Insert None "" [] [] []

mainBuf :: String
mainBuf = "main"
