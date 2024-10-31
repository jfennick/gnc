module Rockets where

import Utils
import Types

getBurnTime :: Double -> [Engine] -> Double
getBurnTime mass_prop engines = time where
    dmdt' = sum $ map dmdt engines
    time = mass_prop / dmdt'

-- CF6-50 Turbofan
cf6_mass :: Double
cf6_mass = 3960.0 -- kg
cf6_TWR :: Double
cf6_TWR = 6.01 -- unitless
cf6_SFC :: Double
cf6_SFC = 0.0105 -- (kg/s)/kN
cf6_thrust :: Double
cf6_thrust = (cf6_mass * littleg) * cf6_TWR -- Newton
cf6_fuel_rate :: Double
cf6_fuel_rate = (cf6_thrust / 1000.0) * cf6_SFC
--println!("CF6_thrust (kN) {:?}", CF6_thrust)
--println!("CF6_thrust (Tons) {:?}", CF6_thrust / (little_g * 1000.0))
--println!("CF6_fuel_rate (kg/s) {:?}", CF6_fuel_rate)
turbofan_CF6 :: Engine
turbofan_CF6 = Engine
    { dmdt = cf6_fuel_rate
    , velocity_exhaust = 95000.0
    , diameter = 2.19
    , mass = cf6_mass
    }

numEnginesExterior :: Floating a => a -> a -> a
numEnginesExterior diameter_rocket diameter_engine =
    -- Close enough for a first approximation.
    pi * (diameter_rocket - diameter_engine) / diameter_engine

triangularNumber :: Int -> Int
triangularNumber n = truncate $ fromIntegral (n * (n + 1)) / 2

circlePackingNumber :: Int -> Int
circlePackingNumber n = 1 + 6 * triangularNumber (n - 1)

--println!("circle_packing_numbers 1 to 13")
--println!({:?}, (1..14).iter().map(|n| circle_packing_number(n)).collect())

-- Rocket / staging setup
crossSectionalArea :: Double
crossSectionalArea = pi * 4.5 * 4.5 -- m^2

r2 :: Engine
r2 = Engine
    { dmdt = 650 -- 626 == 493 + 133 ??
    , velocity_exhaust = 3200.0
    , diameter = 1.3
    , mass = 1630.0 -- 1600?
    }

r2vac :: Engine
r2vac = Engine
    { dmdt = 650.0
    , velocity_exhaust = 3560.0
    , diameter = 2.4
    , mass = 1600.0
    }

superheavy :: Stage
superheavy = stage where
    engs = replicate 33 r2
    massprop = 3400000.0
    burntime = getBurnTime massprop engs
    stage = Stage
            { mass_dry = 200000.0
            , mass_payload = 0.0
            , mass_prop = massprop
            , engines = engs
            , burn_time = 0.85 * burntime -- Save for boostback burn
            }

starshipEngines :: [Engine]
starshipEngines = replicate 3 r2 ++ replicate 6 r2vac

starship :: Stage
starship = stage where
    engs = starshipEngines
    massprop = 1200000.0
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = 100000.0
        , mass_payload = 120000.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = burntime
        }

-- i.e. 12 hexagonal rings of turbofans, with the central turbofan + inner 3 rings removed.
-- This should give about 10% more thrust than superheavy, and about 5X more dry mass. (1425 Tons...)
-- Note that this dry mass estimate does not include any of the structural mass
-- necessary to actually connect the turbofans together and to the rocket, but
-- it also doesn't include any mass savings you would get by designing an
-- optimized turbofan specifically for this application.
numFans :: Int
numFans = circlePackingNumber 12 - circlePackingNumber 4 -- 360 = 397 - 37

turbostage :: Stage
turbostage = stage where
    engs = replicate numFans turbofan_CF6
    massprop = 50000.0
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = fromIntegral numFans * cf6_mass
        , mass_payload = 0.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = burntime
        }

superheavyTurbo :: Stage
superheavyTurbo = stage where
    engs = replicate numFans turbofan_CF6
    massprop = mass_prop superheavy - mass_prop turbostage
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = 200000.0
        , mass_payload = 0.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = 0.85 * burntime -- Save for boostback burn
        }

-- In thrust we trust!
superheavyA :: Stage
superheavyA = stage where
    engs = replicate (33 + 25) r2
    massprop = 2200000.0
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = 200000.0
        , mass_payload = 0.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = 0.91 * burntime -- Save for boostback burn
        }

superheavyB :: Stage
superheavyB = stage where
    engs = replicate 30 r2
    massprop = 1200000.0
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = 100000.0
        , mass_payload = 0.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = 0.77 * burntime -- Save for boostback burn
        }

superheavyATurbo :: Stage
superheavyATurbo = stage where
    engs = replicate (33 + 25) r2
    massprop = mass_prop superheavyA - mass_prop turbostage
    burntime = getBurnTime massprop engs
    stage = Stage
        { mass_dry = 200000.0
        , mass_payload = 0.0
        , mass_prop = massprop
        , engines = engs
        , burn_time = 0.91 * burntime -- Save for boostback burn
        }
