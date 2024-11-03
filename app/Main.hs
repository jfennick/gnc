module Main where

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
        timeStageSep :: Floating a => a
        ctrls_1_bb = contiguousTimes timeStageSep [boostback, coastPhase1, landingBurn13, landingBurn3]
        ctrls_1a = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay, boostback]
        ctrls_1b = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay, boostback]
        ctrls_1, ctrls_1_bb, ctrls_1a, ctrls_1b, ctrls_2 :: Floating a => [Control a]
    --  let ctrls_t = ...
    --  let ctrls_a_t = ...

        ctrls_2 = contiguousTimes timeStageSep $ gravityTurn2 ++ [over12] ++ [coastPhase2]
        -- ctrls_2 = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay] ++ gravityTurn2 ++ [over12]

    -- let stages = [superheavy, starship]
    -- let stages = [turbostage, superheavy_t, starship]
    let stages = [superheavyA, superheavyB, starship]
    -- let stages = [turbostage, superheavy_a_t, superheavy_b, starship]

    let controlsVec = [ctrls_1, ctrls_1_bb, ctrls_2]
        controlsVec :: Floating a => [[Control a]]
    let controlsLengths = map length controlsVec
        controlsLengths :: [Int]
    let paramsVec = toParameterVector $ concat controlsVec
        paramsVec :: Floating a => [a]

    let fn paramsVec = (telemetries, orbit, score) where
            results = runStages controlsLengths paramsVec
            (telemetries, mass_prop_remaining, orbit) = extractData results
            times = map t1 $ subdivideControls dtMax $ ctrls_1 ++ ctrls_2
            score = scoreTelemetries orbit telemetries times mass_prop_remaining 1

    let controls = map (subdivideControls dtMax) [ctrls_1 ++ ctrls_1_bb, ctrls_1 ++ ctrls_2]
    let (telemetries, orbit, score) = fn paramsVec
    putStrLn $ "orbit " ++ show orbit
    putStrLn $ "score " ++ show score

    GUI.Canvas.main controls telemetries score

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
