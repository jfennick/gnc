module Utils where

import Constants

magnitude :: Floating a => a -> a -> a
magnitude x y = sqrt $ x * x + y * y

cart2pol :: (Floating a, RealFloat a) => a -> a -> (a, a)
cart2pol x y = (magnitude x y, atan2 y x) -- rho, phi

pol2cart :: Floating a => a -> a -> (a, a)
pol2cart rho phi = (rho * cos phi, rho * sin phi)

forceGravity :: Floating a => a -> a -> a
forceGravity mass altitude = -- G m M / r ^ 2
    let rad = radius_earth + altitude in
    big_G * mass * mass_earth / (rad * rad)

littleg :: Floating a => a
littleg = forceGravity 1.0 0.0 -- 9.8 m/s^2
-- little_g = 9.7984299667611338691131095401942729949951171875000000000000000000 -- m/s^2

forceDrag :: Floating a => a -> a -> a -> a -> a
forceDrag density speed drag_coefficient cross_sectional_area = -- 1/2 rho v^2 c A
    0.5 * density * (speed * speed) * drag_coefficient * cross_sectional_area

karmanLine :: Floating a => a
karmanLine = 84852.0 -- meters

-- See https://en.wikipedia.org/wiki/Barometric_formula#Density_equations
-- altitude (m), density (kg/m^3), temperature (K), temperature lapse rate (K/m)
barData :: Floating a => [(a, a, a, a)]
barData = [
    (0.00000, 1.22500, 288.15, -0.0065),
    (11000.0, 0.36391, 216.65, 0.0),
    (20000.0, 0.08803, 216.65, 0.001),
    (32000.0, 0.01322, 228.65, 0.0028),
    (47000.0, 0.00143, 270.65, 0.0),
    (51000.0, 0.00086, 270.65, -0.0028),
    (71000.0, 0.000064, 214.65, -0.002),
    (karmanLine, 0.00000, 000.00, 0.0)
    ]

barAlts :: Floating a => [a]
barAlts = map (\(alt,_x,_y,_z) -> alt) barData

lapseZero :: Floating a => a -> Int -> a
lapseZero altitude index = density where
    (alt, rho, temp, _lapse) = barData !! index
    numerator = (-1.0) * littleg * molar_mass_air * (altitude - alt)
    denominator = ideal_gas_constant * temp
    density = rho * exp (numerator / denominator)

lapseNonzero :: Floating a => a -> Int -> a
lapseNonzero altitude index = density where
    (alt, rho, temp, lapse) = barData !! index
    base = temp / (temp + (altitude - alt) * lapse)
    exponent = 1.0 + (littleg * molar_mass_air) / (ideal_gas_constant * lapse)
    density = rho * base ** exponent

barometricDensity :: (Floating a, Ord a) => a -> a
barometricDensity altitude
   | altitude > karmanLine = 0.0 -- If we are already in space
   | altitude >= (barAlts !! 0) && altitude < (barAlts !! 1) = lapseNonzero altitude 0
   | altitude >= (barAlts !! 1) && altitude < (barAlts !! 2) = lapseZero altitude 1
   | altitude >= (barAlts !! 2) && altitude < (barAlts !! 3) = lapseNonzero altitude 2
   | altitude >= (barAlts !! 3) && altitude < (barAlts !! 4) = lapseNonzero altitude 3
   | altitude >= (barAlts !! 4) && altitude < (barAlts !! 5) = lapseZero altitude 4
   | altitude >= (barAlts !! 5) && altitude < (barAlts !! 6) = lapseNonzero altitude 5
   | altitude >= (barAlts !! 6) && altitude < (barAlts !! 7) = lapseNonzero altitude 6
   | otherwise = 0.0 -- If we are already in space
