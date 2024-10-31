module Controls where

import Data.List (unfoldr, scanl')

import Types

-- Controls (piecewise constant, all times are in seconds)
uptime :: Double
uptime = 10.0
numturns1 :: Double
numturns1 = 10
turntime1 :: Double -> Double
turntime1 bt = (bt - uptime) / numturns1
turntime1a :: Double -> Double
turntime1a = turntime1
turntime1b :: Double -> Double
turntime1b bt = bt / numturns1
numturns2 :: Double
numturns2 = 10
turntime2 :: Double
turntime2 = 5.0

-- Simplest possible control: straight up, then straight sideways.
up :: Control
up = Control { t1 = 0.0, t2 = uptime, force_phi = 0.0, force_mag = 1.0 }
over :: Control
over = Control {t1 = uptime, t2 = 1000.0, force_phi = pi / 2.0, force_mag = 1.0}
controlsUpOver :: [Control]
controlsUpOver = [up, over]

piecewiseTurn :: Double -> Double -> Double -> Double -> Double -> [Control]
piecewiseTurn init_time init_angle final_angle numturns turntime = map mkTurn [0..numturns] where
    mkTurn r = Control {
        t1 = init_time + r*turntime,
        t2 = init_time + (r+1.0)*turntime,
        force_phi = init_angle + (final_angle - init_angle)*(r+1.0)/numturns,
        force_mag = 1.0
        }

gravityTurn1 :: Double -> [Control]
gravityTurn1 bt = piecewiseTurn 0.0 0.0 (pi/4.0) numturns1 $ turntime1 bt
gravityTurn1a :: Double -> [Control]
gravityTurn1a bt = piecewiseTurn 0.0 0.0 (pi/6.0) numturns1 $ turntime1a bt
gravityTurn1b :: Double -> [Control]
gravityTurn1b bt = piecewiseTurn 0.0 (pi/6.0) (pi/4.0) numturns1 $ turntime1b bt

stagingTime :: Double
stagingTime = 10.0
stagingDelay :: Control
stagingDelay = Control {t1 = 0.0, t2 = stagingTime, force_phi = 0.0, force_mag = 0.0 }

gravityTurn2 :: [Control]
gravityTurn2 = piecewiseTurn 0.0 (pi/4.0) (pi/2.0) numturns2 turntime2

over12 :: Control
over12 = Control {t1 = 0.0, t2 = 1000.0, force_phi = pi / 2.0, force_mag = 1.0 }
boostback :: Control
boostback = Control {t1 = 0.0, t2 = 40.0, force_phi = - (pi / 2.0), force_mag = 13/33 } -- minus = backward

coastTime1 :: Double
coastTime1 = 170.0
coastPhase1 :: Control
coastPhase1 = Control {t1 = 0.0, t2 = coastTime1, force_phi = 0.0, force_mag = 0.0 }

landingBurn13 :: Control
landingBurn13 =  Control {t1 = 0.0, t2 = 5.0, force_phi = pi / 6.0, force_mag = 13/33 }
landingBurn3 :: Control
landingBurn3 =  Control {t1 = 0.0, t2 = 20.0, force_phi = 0.0, force_mag = 3/33 }

contiguousTimes :: Double -> [Control] -> [Control]
contiguousTimes offset controls = controls' where
    dt c = t2 c - t1 c
    dts = map dt controls
    times = scanl' (+) offset dts
    controls' = zipWith (\c (t1', t2') -> c {t1=t1', t2=t2'}) controls $ zip times $ tail times

subdivideControl :: Double -> Control -> [Control]
subdivideControl dtMax control = controls where
    deltat = t2 control - t1 control
    num = ceiling $ deltat / dtMax -- use ceiling so that dt < dtMax
    dt = deltat / fromIntegral num
    times = map ((+ t1 control) . (* dt)) [0..]
    timeIntervals = zip times $ tail times
    mkControl (t1', t2') = control {t1 = t1', t2 = t2'}
    controls = map mkControl $ take num timeIntervals

subdivideControls :: Double -> [Control] -> [Control]
subdivideControls = concatMap . subdivideControl

perturbControlsPhiRandom :: [Control] -> Double -> Int -> Int -> [Double] -> [Control]
perturbControlsPhiRandom controls scale index_min index_max rands01 = controls' where
    delta_phis = map (\r -> (r * 2.0 - 1.0) * (pi / 2.0) / scale) rands01
    mkControl idx dphi c = if index_min <= idx && idx < index_max
                            then c {force_phi = dphi + force_phi c}
                            else c
    controls' = zipWith3 mkControl [0..] delta_phis controls

perturbControlsDtRandom :: [Control] -> Double -> Int -> Int -> [Double] -> [Control]
perturbControlsDtRandom controls scale index_min index_max rands01 = controls' where
    dts = zipWith (\r c -> (r * 2.0 - 1.0) * (t2 c - t1 c) / scale) rands01 controls
    mkControl idx dt c = if index_min <= idx && idx < index_max
                            then c {t2 = dt + t2 c}
                            else c
    controls' = zipWith3 mkControl [0..] dts controls
