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
    putStrLn $ "burntime " ++ show burntime

    let burntime_a = 0.91 * getBurnTime 2200000.0 (engines superheavyA)
    let burntime_b = 0.77 * getBurnTime 1200000.0 (engines superheavyB)
    let burntime_t = 0.85 * getBurnTime (3400000.0 - 50000.0) (engines superheavy)
    let burntime_a_t = 0.91 * getBurnTime (2200000.0 - 50000.0) (engines superheavyA)

    let ctrls_1 = contiguousTimes 0 $ [up] ++ gravityTurn1 burntime  ++ [stagingDelay]
    let timeStageSep = t2 $ last ctrls_1
    let ctrls_1_bb = contiguousTimes timeStageSep [boostback, coastPhase1, landingBurn13, landingBurn3]
    let ctrls_1a = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay, boostback]
    let ctrls_1b = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay, boostback]
    --  let ctrls_t = ...
    --  let ctrls_a_t = ...

    -- let ctrls_2 = contiguousTimes 0 $ [up] ++ gravityTurn1 burntime1 ++ [stagingDelay] ++ gravityTurn2 ++ [over12]
    let ctrls_2 = contiguousTimes 0 $ [up] ++ gravityTurn1a burntime_a ++ [stagingDelay] ++ gravityTurn1b burntime_b ++ [stagingDelay] ++ gravityTurn2 ++ [over12]

    -- let stages = [superheavy, starship]
    -- let stages = [turbostage, superheavy_t, starship]
    let stages = [superheavyA, superheavyB, starship]
    -- let stages = [turbostage, superheavy_a_t, superheavy_b, starship]

    let controls_boost1 = subdivideControls dtMax ctrls_1 -- $ take (length ctrls_1) controls -- ctrls_1
    let wetmass_starship = mass_dry starship + mass_prop starship + mass_payload starship
    let stage_boost1_init = superheavy {mass_payload = wetmass_starship}
    let results_boost1 = run stage_boost1_init controls_boost1 getInitialTelemetry
    let stage_boost1_final = fst3 $ last results_boost1
    let telemetries_boost1 = map snd3 results_boost1
    let mass_prop_remaining = mass_prop stage_boost1_final
    putStrLn $ "mass_prop_remaining " ++ show mass_prop_remaining

    let controls_boost1_bb = subdivideControls dtMax ctrls_1_bb -- $ drop (length ctrls_1) controls -- ctrls_1_bb
    let results_boost1_bb = run stage_boost1_final {mass_payload = 0.0} controls_boost1_bb $ last telemetries_boost1
    let stage_boost1_bb_final = fst3 $ last results_boost1_bb
    let telemetries_boost1_bb = map snd3 results_boost1_bb
    let mass_prop_remaining_bb = mass_prop stage_boost1_bb_final
    putStrLn $ "mass_prop_remaining_bb " ++ show mass_prop_remaining_bb

    let controls = controls_boost1 ++ controls_boost1_bb
    let telemetries = telemetries_boost1 ++ telemetries_boost1_bb

    let times = map t1 controls_boost1
    let score = scoreTelemetries False [telemetries] times [mass_prop_remaining_bb, mass_prop_remaining_bb] 1
    putStrLn $ "score " ++ show score

    GUI.Canvas.main controls telemetries score
