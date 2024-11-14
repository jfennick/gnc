module Controls where

import Types

-- Controls (piecewise constant, all times are in seconds)
uptime :: Floating a => a
uptime = 10.0
numturns1 :: Int
numturns1 = 10
turntime1 :: Floating a => a -> a
turntime1 bt = (bt - uptime) / fromIntegral numturns1
turntime1a :: Floating a => a -> a
turntime1a = turntime1
turntime1b :: Floating a => a -> a
turntime1b bt = bt / fromIntegral numturns1
numturns2 :: Int
numturns2 = 10
turntime2 :: Floating a => a
turntime2 = 8.0

-- Simplest possible control: straight up, then straight sideways.
up :: Floating a => Control a
up = Control {delta_t = uptime, force_phi = 0.0, force_mag = 1.0 }
over :: Floating a => [Control a]
over = [Control {delta_t = 1000.0 - uptime, force_phi = pi / 2.0, force_mag = 1.0}]
controlsUpOver :: Floating a => [Control a]
controlsUpOver = [up] ++ over

piecewiseTurn :: Floating a => a -> a -> Int -> a -> [Control a]
piecewiseTurn init_angle final_angle numturns turntime = map (mkTurn . fromIntegral) [0..numturns] where
    mkTurn r = Control {
        delta_t = turntime,
        force_phi = init_angle + (final_angle - init_angle)*(r+1.0)/ fromIntegral numturns,
        force_mag = 1.0
        }

gravityTurn1 :: Floating a => a -> [Control a]
gravityTurn1 bt = piecewiseTurn 0.0 (pi/4.0) numturns1 $ turntime1 bt
gravityTurn1a :: Floating a => a -> [Control a]
gravityTurn1a bt = piecewiseTurn 0.0 (pi/6.0) numturns1 $ turntime1a bt
gravityTurn1b :: Floating a => a -> [Control a]
gravityTurn1b bt = piecewiseTurn (pi/6.0) (pi/4.0) numturns1 $ turntime1b bt

stagingTime :: Floating a => a
stagingTime = 10.0
stagingDelay :: Floating a => Control a
stagingDelay = Control {delta_t = stagingTime, force_phi = 0.0, force_mag = 0.0 }

gravityTurn2 :: Floating a => [Control a]
gravityTurn2 = piecewiseTurn (pi/4.0) (pi/2.0) numturns2 turntime2

over12 :: Floating a => [Control a]
over12 = [Control {delta_t = 137.0, force_phi = pi / 2.0, force_mag = 1.0 }]
boostback :: Floating a => [Control a]
boostback = [Control {delta_t = 40.5, force_phi = - (pi / 2.0), force_mag = 13/33 }] -- minus = backward
-- boostback = map (\c -> c {force_mag = 13/33}) $ dropLast $ piecewiseTurn (- (pi/2.0)) (- (pi/2.0)) 10 (40.5 / 10)
dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs

coastPhase1 :: Floating a => [Control a]
coastPhase1 = [Control {delta_t = 180.0, force_phi = 0.0, force_mag = 0.0 }]
-- coastPhase1 = map (\c -> c {force_phi = 0, force_mag = 0}) $ dropLast $ piecewiseTurn 0 0 20 (180 / 20)
coastPhase2 :: Floating a => [Control a]
coastPhase2 = [Control {delta_t = 4500.0, force_phi = 0.0, force_mag = 0.0 }]

landingBurn13 :: Floating a => Control a
landingBurn13 =  Control {delta_t = 5.0, force_phi = 0.0, force_mag = 13/33 }
landingBurn3 :: Floating a => Control a
landingBurn3 =  Control {delta_t = 20.0, force_phi = 0.0, force_mag = 3/33 }

subdivideControl :: (Floating a, RealFrac a) => a -> Control a -> [Control a]
subdivideControl dtMax control = controls where
    deltat = delta_t control
    num = ceiling $ deltat / dtMax -- use ceiling so that dt < dtMax
    dt = deltat / fromIntegral num
    controls = replicate num (control {delta_t = dt})

subdivideControls :: (Floating a, RealFrac a) => a -> [Control a] -> [Control a]
subdivideControls = concatMap . subdivideControl

perturbControlsPhi :: Floating a => [Control a] -> a -> Int -> Int -> [a] -> [Control a]
perturbControlsPhi controls scale index_min index_max rands01 = controls' where
    delta_phis = map (\r -> (r * 2.0 - 1.0) * (pi / 2.0) / scale) rands01
    mkControl idx dphi c = if index_min <= idx && idx < index_max
                            then c {force_phi = dphi + force_phi c}
                            else c
    controls' = zipWith3 mkControl [0..] delta_phis controls

perturbControlsDt :: Floating a => [Control a] -> a -> Int -> Int -> [a] -> [Control a]
perturbControlsDt controls scale index_min index_max rands01 = controls' where
    dts = zipWith (\r c -> (r * 2.0 - 1.0) * (delta_t c) / scale) rands01 controls
    mkControl idx dt c = if index_min <= idx && idx < index_max
                            then c {delta_t = dt + delta_t c}
                            else c
    controls' = zipWith3 mkControl [0..] dts controls

toParameterVector :: Floating a => [Control a] -> [a]
toParameterVector = concatMap (\c -> [delta_t c, force_phi c, force_mag c])

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = go xs [] where
    go [] xss = xss
    go xs' xss = take n xs' : go (drop n xs') xss

chunksOfLens :: [Int] -> [a] -> [[a]]
chunksOfLens ns xs = go ns xs [] where
    go ns' [] xss = xss
    go [] xs' xss = xss
    go ns' xs' xss = take (head ns') xs' : go (tail ns') (drop (head ns') xs') xss

fromParameterVector :: Floating a => [a] -> [Control a]
fromParameterVector params = map (\[p1, p2, p3] -> Control p1 p2 p3) $ chunksOf 3 params

filterViaBitmask :: [Bool] ->[a] -> [a]
filterViaBitmask bools params = map snd $ filter fst $ zip bools params

extendViaBitmask ::  Floating a => [(a, Bool)] -> [a] -> [a]
extendViaBitmask = go where
    go [] _ = []
    go ((p0,b):p0bs) ps = if b
        then head ps : go p0bs (tail ps)
        else p0 : go p0bs ps
