module Main where

import Control.Concurrent.STM ( newTVarIO )

import Dhall ( inputFile, auto )
import Types (Config)
import MainCLI ( mainCLI' )
import GUI.Canvas ( mainGUI' )

main :: IO ()
main = do
    tvar <- newTVarIO ([], [], (0.0, []))
    config <- Dhall.inputFile Dhall.auto "app/config.dhall" :: IO (Config Double)
    print config
    -- mainCLI' tvar config
    mainGUI' tvar config
