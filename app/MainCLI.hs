module MainCLI where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Dhall ( inputFile, auto )

import Numeric.AD (gradientDescent)
import Numeric.AD.Mode.Kahn (grad, gradWith')
-- By default, Numeric.AD (grad, gradWith') gradients are calculated with Reverse mode.
-- For unknown reasons, Reverse mode gradients can easily become NaN, which
-- causes gradientDescent to either immediately return [] or hang.
-- Alternatively, Kahn mode gradients appear to be much more numerically stable.
-- Note that many of the API functions have hardcoded Reverse mode type signatures.
-- However, they can be converted to Kahn mode simply by changing the type signature.
-- The function bodies themselves remain identical.
-- (Both terms AND types determine the behavior of code in Haskell!)

import Controls
import Params
import Simulation
import Types
import Utils

-- import GUI.Canvas qualified
import Data.List (scanl')

fst3 :: (a, b, c) -> a
fst3 (x, _y, _z) = x

snd3 :: (a, b, c) -> b
snd3 (_x, y, _z) = y

trd3 :: (a, b, c) -> c
trd3 (_x, _y, z) = z

trd4 :: (a, b, c, d) -> c
trd4 (_x, _y, z, _w) = z

mainCLI :: IO ()
mainCLI = do
    tvar <- newTVarIO ([], [], (0.0, []))
    config <- Dhall.inputFile Dhall.auto "app/config.dhall" :: IO (Config Double)
    print config
    mainCLI' tvar config

mainCLI' :: (Floating a, RealFloat a, Ord a, Show a) => TVar ([[Control a]], [[Telemetry a]], (a, [a])) -> Config Double -> IO ()
mainCLI' tvar config = do
    let config' = fmap realToFrac config
        config' :: Floating a => Config a

    let dtMax' :: Floating a => a
        dtMax' = dtMax config' -- integration timestep, seconds
    let accLimit' :: Floating a => a
        accLimit' = littleg * (accLimit config')

    let stages' = stages config'
        stages' :: Floating a => [Stage a]
    let controls' = controls config'
        controls' :: Floating a => [[Controls a]]
    let controlsVec = mkControlsVec controls'
        controlsVec :: Floating a => [[Control a]]
    print "controlsVec"
    mapM_ (mapM_ print) controlsVec

    let [ctrls_1, ctrls_1_bb, ctrls_2] = controlsVec

    let controlsLengths = map length controlsVec
        controlsLengths :: [Int]
    let optBitmask = concat $ replicate (length $ concat controlsVec) [False, True, False]
        -- i.e. selectively optimize over force_phi only; keep all other control parameters fixed
        -- TODO: more forms of dimensional reduction and/or selective parameter rescaling
        optBitmask :: [Bool]
    let paramsVec0 = toParameterVector $ concat controlsVec
        paramsVec0 :: Floating a => [a]
    let paramsVec0Enabled = filterViaBitmask optBitmask paramsVec0
        paramsVec0Enabled :: Floating a => [a]

    let fn paramsVec = (telemetries, orbit, (score, components), mass_prop_remaining) where
            paramsVec' = extendViaBitmask (zip paramsVec0 optBitmask) paramsVec

            [ctrls_1_, ctrls_1_bb_, ctrls_2_] = chunksOfLens controlsLengths $ fromParameterVector paramsVec'
            controls = [ctrls_1_, ctrls_1_bb_, ctrls_2_]

            results = runStages accLimit' dtMax' stages' controls
            (telemetries, mass_prop_remaining, orbit) = extractData results
            times = scanl' (+) 0 $ map delta_t $ subdivideControls dtMax' $ ctrls_1_ ++ ctrls_2_
            score = scoreTelemetries orbit telemetries times mass_prop_remaining 1
            (_, _, components) = scoreComponents telemetries times mass_prop_remaining 1
        fn' = negate . fst . trd4 . fn
        fn' :: (Floating a, RealFloat a) => [a] -> a


    let (fx0, xgx0) = gradWith' (,) fn' paramsVec0Enabled
    putStrLn $ "fx0 " ++ show fx0
    mapM_ print $ zip paramsVec0Enabled xgx0

    let (telemetries0, orbit0, (score0, components0), mass_prop_remaining0) = fn paramsVec0Enabled
    let controls0 = map (subdivideControls dtMax') [ctrls_1 ++ ctrls_1_bb, ctrls_1 ++ ctrls_2]
    tvar <- newTVarIO (controls0, [], (0.0, replicate (length components0) 0.0))

    gdAsync <- async $ do
        let paramsVecsEnabled = take 100 $ gradientDescent fn' paramsVec0Enabled
        forM_ paramsVecsEnabled $ \paramsVecEnabled -> do
            print paramsVecEnabled
            let (fx0, xgx0) = gradWith' (,) fn' paramsVecEnabled
            putStrLn $ "-score " ++ show fx0
            mapM_ print $ zip paramsVecEnabled xgx0

            let paramsVec' = extendViaBitmask (zip paramsVec0 optBitmask) paramsVecEnabled
            let [ctrls_1', ctrls_1_bb', ctrls_2'] = chunksOfLens controlsLengths $ fromParameterVector paramsVec'
            let controlsSub = map (subdivideControls dtMax') [ctrls_1' ++ ctrls_1_bb', ctrls_1' ++ ctrls_2']
            let (telemetries, orbit, (score, components), mass_prop_remaining) = fn paramsVecEnabled
            putStrLn $ "orbit " ++ show orbit
            atomically $ writeTVar tvar (controlsSub, telemetries, (score, components))

    -- GUI.Canvas.mainGUI' tvar config)
    -- wait gdAsync
    pure ()

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

runStages :: (Floating a, RealFloat a) => a -> a -> [Stage a] -> [[Control a]] -> Results a
runStages accLimit dtMax stages controls = results where
    [superheavy, starship] = stages
    [ctrls_1, ctrls_1_bb, ctrls_2] = controls

    controls_boost1 = subdivideControls dtMax ctrls_1
    wetmass_starship = mass_dry starship + mass_prop starship + mass_payload starship
    stage_boost1_init = superheavy {mass_payload = wetmass_starship}
    results_boost1 = run accLimit stage_boost1_init controls_boost1 getInitialTelemetry
    stage_boost1_final = fst3 $ last results_boost1
    telemetries_boost1 = map snd3 results_boost1

    controls_boost1_bb = subdivideControls dtMax ctrls_1_bb
    results_boost1_bb = run accLimit stage_boost1_final {mass_payload = 0.0} controls_boost1_bb $ last telemetries_boost1

    controls_upper = subdivideControls dtMax ctrls_2
    results_upper = run accLimit starship controls_upper $ last telemetries_boost1

    results = [[results_boost1, results_boost1_bb], [results_boost1, results_upper]]
