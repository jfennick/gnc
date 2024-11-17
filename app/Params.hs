module Params where

import Constants
import Utils
import Types
import Data.List (scanl')

-- Simulation setup
-- Parameters

getInitialTelemetry :: (Floating a, Ord a) => Telemetry a
getInitialTelemetry = telem where
    telem = Telemetry {
        position = Coords { x = radius_earth, y = 0.0 },
        velocity = Coords { x = 0.0, y = tangental_velocity_earth },
        acceleration = Coords { x = 0.0, y = 0.0 },
        barometric_density = barometricDensity 0.0,
        dynamic_pressure = 0.0
        }

getDownranges :: Floating a => [a] -> [a] -> [a]
getDownranges phis times = downranges where
    downranges = zipWith (\time phi -> (radius_earth * phi - tangental_velocity_earth * time) / 1000.0) times phis
