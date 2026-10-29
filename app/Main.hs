module Main where

import Control.Concurrent.Async ( async, link )
import Control.Concurrent.STM ( TVar, newTVarIO )

import Dhall ( inputFile, auto )
import Types (Config, Control, Telemetry)
import MainCLI ( mainCLI' )
import GUI.Canvas ( mainGUI' )

main :: IO ()
main = do
    tvar <- newTVarIO ([], [], (0.0, [])) :: IO (TVar ([[Control Double]], [[Telemetry Double]], (Double, [Double])))
    config <- Dhall.inputFile Dhall.auto "app/config.dhall" :: IO (Config Double)
    print config
    -- Run the optimizer in the background; it writes each iteration into tvar,
    -- which the GUI polls. link rethrows any optimizer exception in this thread.
    cli <- async $ mainCLI' tvar config
    link cli
    mainGUI' tvar config
