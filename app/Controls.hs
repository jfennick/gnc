module Controls where

import Types

validateControls :: Floating a => [[Controls a]] -> Bool
validateControls controls = isValid where
    atMostOneStaging = all (\cs -> length (map isStaging cs) <= 1) controls
    isValid = atMostOneStaging

mkControls :: Floating a => Controls a -> [Control a]
mkControls (Controls controls) = controls
mkControls (PieceWise (PieceWiseTurn init_angle final_angle numturns turntime)) = piecewiseTurn init_angle final_angle numturns turntime
mkControls (NullStaging delta_t) = [Control delta_t 0.0 0.0]
mkControls (NullCoast delta_t) = [Control delta_t 0.0 0.0]

mkControlsVec :: Floating a => [[Controls a]] -> [[Control a]]
mkControlsVec [boost_, ctrls_2] = controlsVec where
    boost = takeWhile (not . isStaging) boost_
    boost' = dropWhile (not . isStaging) boost_
    ctrls_1 = boost ++ [head boost']
    ctrls_1_bb = tail boost'
    controlsVec = map (concatMap mkControls) [ctrls_1, ctrls_1_bb, ctrls_2]
mkControlsVec _ = undefined -- TODO

piecewiseTurn :: Floating a => a -> a -> Int -> a -> [Control a]
piecewiseTurn init_angle final_angle numturns turntime = map (mkTurn . fromIntegral) [0..numturns-1] where
    mkTurn r = Control {
        delta_t = turntime / fromIntegral numturns,
        force_phi = init_angle + (final_angle - init_angle)*(r+1.0)/ fromIntegral numturns,
        force_mag = 1.0
        }

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
