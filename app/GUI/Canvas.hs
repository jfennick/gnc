module GUI.Canvas where

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Params
import Simulation
import Types
import Utils (littleg, magnitude)

main :: [Control] -> [Telemetry] -> Double -> IO ()
main controls telemetries score = do
    startGUI defaultConfig (setup controls telemetries score)

canvasSize :: Int
canvasSize = 270
canvasSized :: Double
canvasSized = fromIntegral canvasSize

rescale :: Double -> Double -> [Double] -> [Double]
rescale zmin' zmax' zs = zs' where
    zmin = minimum zs
    zmax = maximum zs
    scale = abs $ (zmax' - zmin') / (zmax - zmin)
    offset = zmin' - zmin
    zs' = map (\z -> (z + offset) * scale) zs

getCanvas :: UI Element
getCanvas = UI.canvas
    # set UI.height canvasSize
    # set UI.width  canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]

canvasGrid :: Int -> Int -> UI [[Element]]
canvasGrid rows cols = forM [1..rows] (\_r -> forM [1..cols] (\_c -> getCanvas))

setup :: [Control] -> [Telemetry] -> Double -> Window -> UI ()
setup controls telemetries score window = do
    let xs = map (x . position) telemetries
    let ys = map (y . position) telemetries
    let times = map t1 controls
    let downranges = getDownranges (map getPhi telemetries) times
    let altitudes = map getAltitude telemetries
    let velocities = map getVelocity telemetries
    let accelerations = map (\c -> magnitude (x c) (y c) / littleg) $ map acceleration telemetries
    let control_phis = map force_phi controls
    let dynamic_pressures = map dynamic_pressure telemetries

    return window # set title "Plot the things"

    canvases <- canvasGrid 2 7
    let canvas1 = head canvases !! 0
    let canvas2 = head canvases !! 1
    let canvas3 = head canvases !! 2
    let canvas4 = head canvases !! 3
    let canvas5 = head canvases !! 4
    let canvas6 = head canvases !! 5
    let canvas7 = head canvases !! 6

    drawPoints <- UI.button #+ [string "Do the thing."]
    clear     <- UI.button #+ [string "Clear the canvas."]

    getBody window #+
        [column $ map (row . map element) canvases
        , element drawPoints, element clear
        ]

    on UI.click clear $ const $
        forM_ (head canvases) (\canvas -> canvas # UI.clearCanvas)

    -- draw some points
    on UI.click drawPoints $ const $ do
        -- canvas # UI.strokeText ("score " ++ show score) (fromIntegral canvasSize / 2 , fromIntegral canvasSize)

        plotPoints canvas1 times altitudes
        plotPoints canvas2 times velocities
        plotPoints canvas3 times accelerations
        plotPoints canvas4 times control_phis
        plotPoints canvas5 times dynamic_pressures
        plotPoints canvas6 xs ys
        plotPoints canvas7 downranges altitudes

showDouble :: Int -> Double -> [Char]
showDouble n d = take n $ show $ d / 1000.0

plotPoints :: UI.Canvas -> [Double] -> [Double] -> UI ()
plotPoints canvas xs ys = do
    let (xmin, xmax) = (minimum xs, maximum xs)
    let (ymin, ymax) = (minimum ys, maximum ys)
    let ticksX = zip [1..10] $ map (* (canvasSized / 10)) [1..10]
    let ticksY = zip [1..10] $ map (canvasSized -) $ map (* (canvasSized / 10)) [1..10]
    forM_ ticksX $ \(idx, tickX) -> do
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        canvas # UI.fillRect (tickX,canvasSized * 0.95) 1 (canvasSized * 0.05)
        canvas # UI.strokeText (showDouble 5 $ (xmax - xmin) * idx + xmin) (tickX,canvasSized * 0.95)
    forM_ ticksY $ \(idx, tickY) -> do
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        canvas # UI.fillRect (0,tickY) (canvasSized * 0.05) 1
        canvas # UI.strokeText (showDouble 5 $ (ymax - ymin) * idx + ymin) (0, tickY)
    -- todo draw tick marks / labels
    let xs' = rescale 0 canvasSized xs
    let ys' = rescale 0 canvasSized ys
    let ys'' = map (canvasSized -) ys' -- +y axis points down
    let points = zipWith (\x y -> (x, y, 1, 1, "black")) xs' ys''
    forM_ points $ \(x,y,w,h,color) -> do
        canvas # set' UI.fillStyle (UI.htmlColor color)
        canvas # UI.fillRect (x,y) w h
