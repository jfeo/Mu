module Main where

import Mu.Part
import Mu.Types
import Mu.Core.Buffers
import Mu.Core.Parts
import Mu.Core.Shipped
import Data.Default

-- Sker der

-- | Loops through all commands - in order - forever.
loop :: Mu -> IO ()
loop mu = do
    mu' <- runInOrder mu
    loop mu'

-- | The main function!
main :: IO ()
main = do
    -- Create the starting mu value
    let mu = def { muActive  = mainBufName
                 , muBuffers = coreBuffers
                 , muParts   = coreParts
                             ++ shippedParts
                 , muVars = [ Var "mainbuffer"    mainBufName
                            , Var "commandBuffer" commandBufName
                            , Var "statusBuffer"  statusBufName
                            ]
                 }
    -- Set info and display, so the user sees something right away.
    mu' <-  runPartsByName ["core_info", "core_display"] mu
    -- Start looping
    loop mu'
