module Constants where

mass_earth :: Double
mass_earth = 5.972e+24 -- kg
radius_earth :: Double
radius_earth = 6378000.0 -- m
big_G :: Double
big_G = 6.6743e-11 -- m^3 kg^-1 s^-2
-- little_g in utils.hs
ideal_gas_constant :: Double
ideal_gas_constant = 8.3144598 -- N·m/(mol·K)
molar_mass_air :: Double
molar_mass_air = 0.0289644 -- kg/mol
tangental_velocity_earth :: Double
tangental_velocity_earth =
    (radius_earth * 2.0 * pi) / (60.0 * 60.0 * 24.0) -- 463.8 m/s
standard_pressure :: Double
standard_pressure = 101325.0 -- Pascal = N / m^2
speed_of_sound :: Double
speed_of_sound = 343.0 -- m/s

-- NOTE: The humble .sqrt() method is defined by cases (pos & neg), so
-- it cannot be used to define global constants; define the square instead.
velocity_escape_2 :: Double
velocity_escape_2 = 2.0 * big_G * mass_earth / radius_earth