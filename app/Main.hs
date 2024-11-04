module Main where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (forM_)

import Numeric.AD

import Controls
import Params
import Rockets
import Simulation
import Types

import GUI.Canvas qualified

fst3 :: (a, b, c) -> a
fst3 (x, _y, _z) = x

snd3 :: (a, b, c) -> b
snd3 (_x, y, _z) = y

trd3 :: (a, b, c) -> c
trd3 (_x, _y, z) = z

main :: IO ()
main = do
    let burntime = 120 -- 150? -- 134 == 0.85 * getBurnTime 3400000.0 (engines superheavy)
        burntime_a = 0.91 * getBurnTime 2200000.0 (engines superheavyA)
        burntime_b = 0.77 * getBurnTime 1200000.0 (engines superheavyB)
        burntime_t = 0.85 * getBurnTime (3400000.0 - 50000.0) (engines superheavy)
        burntime_a_t = 0.91 * getBurnTime (2200000.0 - 50000.0) (engines superheavyA)
        burntime, burntime_a, burntime_b, burntime_t, burntime_a_t :: Floating a => a
    putStrLn $ "burntime " ++ show burntime

    let ctrls_1 = contiguousTimes 0 $ [up] ++ gravityTurn1 burntime ++ [stagingDelay]
        timeStageSep = t2 $ last ctrls_1
        timeStageSep, timeLandingBurn :: Floating a => a
        ctrls_1_bb0 = contiguousTimes timeStageSep $ boostback ++ [coastPhase1]
        timeLandingBurn = t2 $ last ctrls_1_bb0
        ctrls_1_bb1 = contiguousTimes timeLandingBurn [landingBurn13, landingBurn3]
        ctrls_1_bb = ctrls_1_bb0 ++ ctrls_1_bb1
        ctrls_1a = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ boostback
        ctrls_1b = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay] ++ boostback
        ctrls_1, ctrls_1_bb, ctrls_1_bb0, ctrls_1_bb1, ctrls_1a, ctrls_1b, ctrls_2 :: Floating a => [Control a]
    --  let ctrls_t = ...
    --  let ctrls_a_t = ...

        ctrls_2 = contiguousTimes timeStageSep $ gravityTurn2 ++ [over12] -- ++ [coastPhase2]
        -- ctrls_2 = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay] ++ gravityTurn2 ++ [over12]

    -- let stages = [superheavy, starship]
    -- let stages = [turbostage, superheavy_t, starship]
    let stages = [superheavyA, superheavyB, starship]
    -- let stages = [turbostage, superheavy_a_t, superheavy_b, starship]

    let controlsVec = [ctrls_1, ctrls_1_bb, ctrls_2]
        -- The t1, t2, and force_mag parameters of ctrls_1 gradients are all NaN, which
        -- causes gradientDescent to either immediately return [] or hang.
        -- Moreover, including [coastPhase2] in ctrls_2 also yields []...
        -- For now, just exclude them from optimization using optBitMask.
        controlsVec :: Floating a => [[Control a]]
    let controlsLengths = map length controlsVec
        controlsLengths :: [Int]
    let optBitmask = concat $ replicate (length $ concat controlsVec) [False, False, True, False]
        -- i.e. selectively optimize over force_phi only; keep all other control parameters fixed
        -- TODO: more forms of dimensional reduction and/or selective parameter rescaling
        optBitmask :: [Bool]
    let paramsVec0 = toParameterVector $ concat controlsVec
        paramsVec0 :: Floating a => [a]
    let paramsVec0Enabled = filterViaBitmask optBitmask paramsVec0
        paramsVec0Enabled :: Floating a => [a]

    let fn paramsVec = (telemetries, orbit, score) where
            paramsVec' = extendViaBitmask (zip paramsVec0 optBitmask) paramsVec
            results = runStages controlsLengths paramsVec'
            (telemetries, mass_prop_remaining, orbit) = extractData results
            times = map t1 $ subdivideControls dtMax $ ctrls_1 ++ ctrls_2
            score = scoreTelemetries orbit telemetries times mass_prop_remaining 1
        fn' = negate . trd3 . fn
        fn' :: (Floating a, RealFloat a) => [a] -> a

    let (fx0, xgx0) = gradWith' (,) fn' paramsVec0Enabled
    putStrLn $ "fx0 " ++ show fx0
    mapM_ print $ zip paramsVec0Enabled xgx0

    let controls_init = map (subdivideControls dtMax) [ctrls_1 ++ ctrls_1_bb, ctrls_1 ++ ctrls_2]
    tvar <- newTVarIO (controls_init, [], 0)

    gdAsync <- async $ do
        let paramsVecsEnabled = take 100 $ gradientDescent fn' paramsVec0Enabled
        forM_ paramsVecsEnabled $ \paramsVecEnabled -> do
            print paramsVecEnabled
            let (fx0, xgx0) = gradWith' (,) fn' paramsVecEnabled
            putStrLn $ "-score " ++ show fx0
            mapM_ print $ zip paramsVecEnabled xgx0

            let paramsVec' = extendViaBitmask (zip paramsVec0 optBitmask) paramsVecEnabled
            let [ctrls_1', ctrls_1_bb', ctrls_2'] = chunksOfLens controlsLengths $ fromParameterVector paramsVec'
            let controls = map (subdivideControls dtMax) [ctrls_1' ++ ctrls_1_bb', ctrls_1' ++ ctrls_2']
            let (telemetries, orbit, score) = fn paramsVecEnabled
            putStrLn $ "orbit " ++ show orbit
            atomically $ writeTVar tvar (controls, telemetries, score)

    GUI.Canvas.main tvar
    wait gdAsync

type Results a = (Floating a, RealFloat a) => [[[(Stage a, Telemetry a, Bool)]]]

extractData :: (Floating a, RealFloat a) => Results a -> ([[Telemetry a]], [a], Bool)
extractData results = tup where
    results_boost1 = head $ head results
    telemetries_boost1 = map snd3 results_boost1

    results_boost1_bb = last $ head results
    stage_boost1_bb_final = fst3 $ last results_boost1_bb
    telemetries_boost1_bb = map snd3 results_boost1_bb
    mass_prop_remaining_bb = mass_prop stage_boost1_bb_final

    results_upper = last $ last results
    stage_upper_final = fst3 $ last results_upper
    telemetries_upper = map snd3 results_upper
    mass_prop_remaining_upper = mass_prop stage_upper_final

    telemetries_boost1' = telemetries_boost1 ++ telemetries_boost1_bb
    telemetries_upper' = telemetries_boost1 ++ telemetries_upper
    telemetries = [telemetries_boost1', telemetries_upper']

    mass_prop_remaining = [mass_prop_remaining_bb, mass_prop_remaining_upper]
    orbit = trd3 $ last results_upper
    tup = (telemetries, mass_prop_remaining, orbit)

runStages :: (Floating a, RealFloat a) => [Int] -> [a] -> Results a
runStages chunkLengths paramsVec = results where
    [ctrls_1, ctrls_1_bb, ctrls_2] = chunksOfLens chunkLengths $ fromParameterVector paramsVec

    controls_boost1 = subdivideControls dtMax ctrls_1
    wetmass_starship = mass_dry starship + mass_prop starship + mass_payload starship
    stage_boost1_init = superheavy {mass_payload = wetmass_starship}
    results_boost1 = run stage_boost1_init controls_boost1 getInitialTelemetry
    stage_boost1_final = fst3 $ last results_boost1
    telemetries_boost1 = map snd3 results_boost1

    controls_boost1_bb = subdivideControls dtMax ctrls_1_bb
    results_boost1_bb = run stage_boost1_final {mass_payload = 0.0} controls_boost1_bb $ last telemetries_boost1

    controls_upper = subdivideControls dtMax ctrls_2
    results_upper = run starship controls_upper $ last telemetries_boost1

    results = [[results_boost1, results_boost1_bb], [results_boost1, results_upper]]
