module Params where

import Constants
import Utils
import Types
import Data.List (scanl')

-- Simulation setup
-- Parameters
dtMax :: Double
dtMax = 0.5 -- integration timestep, seconds
timeLimit :: Double
timeLimit = 10000.0 -- seconds
numTimesteps :: Int
numTimesteps = truncate $ timeLimit / dtMax
accelerationLimit :: Double
accelerationLimit = 4.0 * littleg

getInitialTelemetry :: Telemetry
getInitialTelemetry = telem where
    telem = Telemetry {
        position = Coords { x = radius_earth, y = 0.0 },
        velocity = Coords { x = 0.0, y = tangental_velocity_earth },
        acceleration = Coords { x = 0.0, y = 0.0 },
        barometric_density = barometricDensity 0.0,
        dynamic_pressure = 0.0
        }

getStageSepTimes :: [Stage] -> [Double]
getStageSepTimes stages = sep_times where
    burn_times = map burn_time stages
    sep_times = scanl' (+) 0 burn_times

getStageTimeIntervals :: [Stage] -> [(Double, Double)]
getStageTimeIntervals stages = times where
    sep_times = getStageSepTimes stages
    zipped = zip sep_times $ tail sep_times
    times = (0.0, head sep_times) : zipped

getDownranges :: [Double] -> [Double] -> [Double]
getDownranges phis times = downranges where
    downranges = zipWith (\time phi -> (radius_earth * phi - tangental_velocity_earth * time) / 1000.0) times phis
