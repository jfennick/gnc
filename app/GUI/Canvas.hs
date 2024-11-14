module GUI.Canvas where

import Control.Monad (forM_, forM)
import Control.Concurrent.STM

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Params
import Simulation
import Types
import Utils (littleg, magnitude)
import Numeric (showFFloat)
import Data.List (scanl')

type UIData a = (Floating a, RealFloat a, Show a) => TVar ([[Control a]], [[Telemetry a]], (a, [a]))

main :: (Floating a, RealFloat a, Show a) => UIData a -> IO ()
main tvar = do
    startGUI defaultConfig (setup tvar)

canvasSize :: Int
canvasSize = 270
canvasSized :: Floating a => a
canvasSized = fromIntegral canvasSize

rescale :: (Floating a, Ord a) => a -> a -> [a] -> [a]
rescale zmin' zmax' zs = zs' where
    zmin = minimum zs
    zmax = maximum zs
    scale = abs $ (zmax' - zmin') / (zmax - zmin)
    offset = zmin' - zmin
    zs' = map (\z -> (z + offset) * scale) zs

getCanvas :: Int -> Int -> UI Element
getCanvas sizeX sizeY = UI.canvas
    # set UI.height sizeX
    # set UI.width  sizeY
    # set style [("border", "solid black 1px"), ("background", "#eee")]

canvasGrid :: Int -> Int -> UI [[Element]]
canvasGrid rows cols = forM [1..rows] (\_r -> forM [1..cols] (\_c -> getCanvas canvasSize canvasSize))

plotTelems :: (Floating a, RealFloat a) => ([Control a], [Telemetry a], [UI.Canvas]) -> UI ()
plotTelems (controls, telemetries, canvases) = do
    let xs = map (/ 1000.0) $ map (x . position) telemetries
    let ys = map (/ 1000.0) $ map (y . position) telemetries
    let times = scanl' (+) 0 $ map delta_t controls
    let downranges = getDownranges (map getPhi telemetries) times
    let altitudes = map (/ 1000.0) $ map getAltitude telemetries
    let velocities = map (/ 1000.0) $ map getVelocity telemetries
    let accelerations = map (\c -> magnitude (x c) (y c) / littleg) $ map acceleration telemetries
    let control_phis = map force_phi controls
    let dynamic_pressures = map dynamic_pressure telemetries

    let canvas1 = canvases !! 0
    let canvas2 = canvases !! 1
    let canvas3 = canvases !! 2
    let canvas4 = canvases !! 3
    let canvas5 = canvases !! 4
    let canvas6 = canvases !! 5
    let canvas7 = canvases !! 6

    plotPoints canvas1 times altitudes
    plotPoints canvas2 times velocities
    plotPoints canvas3 times accelerations
    plotPoints canvas4 times control_phis
    plotPoints canvas5 times dynamic_pressures
    plotPoints canvas6 xs ys
    plotPoints canvas7 downranges altitudes

setup :: (Floating a, RealFloat a, Show a) => UIData a -> Window -> UI ()
setup tvar window = do
    tup0 <- liftIO $ readTVarIO tvar
    let (controls_all, telemetries_all, (score, components)) = tup0

    return window # set title "I hope u like rockets"

    canvases <- canvasGrid (length controls_all) (length $ head controls_all)
    canvasTextBox <- getCanvas (25 * 7) 200 -- length components

    -- launchButton <- UI.button #+ [string "Optimize launch parameters"]

    getBody window #+
        [column $ map (row . map element) canvases
        -- , element launchButton
        , element canvasTextBox
        ]

    -- draw some points
    -- on UI.click launchButton $ const (foreverUI canvases canvasTextBox tvar)
    foreverUI canvases canvasTextBox tvar

foreverUI :: (Show a, RealFloat a) => [[UI.Canvas]] -> Element -> UIData a -> UI ()
foreverUI canvases canvasTextBox tvar = do
    tup0 <- liftIO $ readTVarIO tvar
    tvar' <- liftIO $ newTVarIO tup0
    go tvar' where
    go tvar' = do
        -- Use STM retry trick to wait until tup has updated
        tup <- liftIO $ atomically $ do
            tup <- readTVar tvar
            tup' <- readTVar tvar'
            if tup == tup' then retry else do
                writeTVar tvar' tup
                pure tup

        forM_ canvases $ \canvasrow ->
            forM_ canvasrow UI.clearCanvas

        let (controls_all, telemetries_all, (score, components)) = tup

        UI.clearCanvas canvasTextBox
        canvasTextBox # UI.strokeText ("score " ++ show score) (0,25)
        forM_ (zip [2..] components) $ \(i, component) -> do
            canvasTextBox # UI.strokeText (show component) (0,5 + i*20)

        mapM_ plotTelems $ zip3 controls_all telemetries_all canvases

        go tvar'

showDouble :: (Floating a, RealFloat a) => Int -> a -> [Char]
showDouble n d = showFFloat (Just n) d ""

plotPoints :: (Floating a, Ord a, RealFloat a) => UI.Canvas -> [a] -> [a] -> UI ()
plotPoints canvas xs ys = do
    let (xmin, xmax) = (minimum xs, maximum xs)
    let (ymin, ymax) = (minimum ys, maximum ys)
    let n = 10 :: Int
    let na = fromIntegral n -- :: a
    let nd = fromIntegral n :: Double
    let ticksX = zip [1..n] $ map (* (canvasSized / nd)) $ map fromIntegral [1..n]
    let ticksY = zip [1..n] $ map (canvasSized -) $ map (* (canvasSized / nd)) $ map fromIntegral [1..n]
    forM_ ticksX $ \(idx, tickX) -> do
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        canvas # UI.fillRect (tickX,canvasSized * 0.95) 1 (canvasSized * 0.05)
        canvas # UI.strokeText (showDouble 2 $ (xmax - xmin) / na * (fromIntegral idx) + xmin) (tickX,canvasSized * 0.95)
    forM_ ticksY $ \(idx, tickY) -> do
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        canvas # UI.fillRect (0,tickY) (canvasSized * 0.05) 1
        canvas # UI.strokeText (showDouble 2 $ (ymax - ymin) / na * (fromIntegral idx) + ymin) (0, tickY)
    -- todo draw tick marks / labels
    let xs' = rescale 0 canvasSized xs
    let ys' = rescale 0 canvasSized ys
    let ys'' = map (canvasSized -) ys' -- +y axis points down
    let points = zipWith (\x y -> (x, y, 1, 1, "black")) xs' ys''
    forM_ points $ \(x,y,w,h,color) -> do
        canvas # set' UI.fillStyle (UI.htmlColor color)
        canvas # UI.fillRect (realToFrac x, realToFrac y) w h
