module Simulation where

import Constants
import Params
import Rockets
import Types
import Utils

getForces :: (Floating a, Ord a, RealFloat a) => a -> Stage a -> Control a -> Bool -> (a, a, a, a, a) -> (a, a, a, a)
getForces mass_stages stage control orbit (mass_prop_remaining, pos_x, pos_y, vel_x, vel_y) = forces where
    mass_curr = mass_stages + mass_prop_remaining
    (pos_rho, pos_phi) = cart2pol pos_x pos_y
    radius = magnitude pos_x pos_y

    -- The velocity of the atmosphere points
    -- in the direction of the derivative of position, i.e. + pi / 2
    phi = pos_phi + pi / 2.0
    atmosphere_vel_phi = tangental_velocity_earth*(pos_rho/radius_earth)
    atm_vel_x = cos phi * atmosphere_vel_phi
    atm_vel_y = sin phi * atmosphere_vel_phi
    -- NOTE: The speed used to calculate forceDrag should be the NET
    -- speed of the stage w.r.t. the atmosphere, NOT just the stage itself!
    -- speed_stage = magnitude vel_x vel_y
    net_vel_x = vel_x - atm_vel_x
    net_vel_y = vel_y - atm_vel_y
    speed = magnitude net_vel_x net_vel_y

    altitude = radius - radius_earth

    -- Determine forces

    -- Force of gravity always points 'down',
    -- in the opposite direction to position (in polar coordinates), i.e. - math.pi
    phi' = pos_phi - pi
    force_gravity_mag = forceGravity mass_curr altitude
    force_gravity_x = cos phi' * force_gravity_mag
    force_gravity_y = sin phi' * force_gravity_mag

    -- Centripetal force always points 'up',
    -- in the same direction to position (in polar coordinates)
    -- Necessary in cartesian?

    -- Force of drag always points
    -- in the opposite direction to velocity, i.e. (-1) *
    density = barometricDensity altitude
    drag_coef = if speed < speed_of_sound then 0.05 else 0.15
    forceDragMag = forceDrag density speed drag_coef crossSectionalArea
    forceDrag_x = if speed == 0.0 then 0.0 else (-1.0) * forceDragMag * net_vel_x / speed
    forceDrag_y = if speed == 0.0 then 0.0 else (-1.0) * forceDragMag * net_vel_y / speed

    forces_sum_x = force_gravity_x + forceDrag_x
    forces_sum_y = force_gravity_y + forceDrag_y

    (tx, ty, tmag) = if mass_prop_remaining > 0.0 && not orbit
        then thrust_tuple
        else (0.0, 0.0, 0.0) where
        thrust_factor = force_mag control -- i.e. throttle

        thrust_mag = thrust_factor * sum (zipWith (*) (map dmdt $ engines stage) (map velocity_exhaust $ engines stage))
        -- To find the thrust vector, add the position and control angles
        phi'' = pos_phi + force_phi control
        thrust_x = cos phi'' * thrust_mag
        thrust_y = sin phi'' * thrust_mag

        --forces_sum_mag = magnitude(forces_sum_rho, forces_sum_phi)
        -- Include all forces, or just thrust? Let's just use thrust.
        -- For the purpose of limiting structural loads we can ignore gravity,
        -- and the acceleration near maxQ is nowhere near this limit.
        acceleration_mag = thrust_mag / mass_curr
        acc_factor = accelerationLimit / acceleration_mag

        --thrust_factor = 1.0
        thrust_tuple = if acc_factor >= 1.0
            then (thrust_x, thrust_y, thrust_factor)
            else (thrust_x', thrust_y', thrust_factor') where
                excess_thrust = (acceleration_mag - accelerationLimit) * mass_curr
                thrust_factor' = (thrust_mag - excess_thrust) / thrust_mag
                thrust_x' = thrust_x * acc_factor
                thrust_y' = thrust_y * acc_factor

    forces = (forces_sum_x + tx, forces_sum_y + ty, forceDragMag, tmag)

getAcceleration :: Floating a => (a, a, a, a) -> a -> Coords a
getAcceleration forces mass = acc where
    (forces_sum_x, forces_sum_y, _forceDragMag, _thrust_factor) = forces
    acc = Coords {x = forces_sum_x / mass, y = forces_sum_y / mass}

step :: (Floating a, Ord a, RealFloat a) => (Stage a, Telemetry a, Bool) -> Control a -> (Stage a, Telemetry a, Bool)
step (stage, telem, reached_orbit_before) control = (stage', telem', reached_orbit_now) where
    (stage', telem') = integrateRungeKutta4 stage control reached_orbit_before telem

    (perihelion, _apohelion, _eccentricity_squared) = visviva (position telem') (velocity telem')
    perihelion_nominal = 200000.0
    reached_orbit_now = perihelion > radius_earth + perihelion_nominal

run :: (Floating a, Ord a, RealFloat a) => Stage a -> [Control a] -> Telemetry a -> [(Stage a, Telemetry a, Bool)]
run stage controls telemInit = resultsShortCircuit where
    -- controls is assumed to contain disjoint contiguous time intervals (NOT necessarily uniform dt!)
    -- tstart <- getCurrentTime
    -- tfinal <- getCurrentTime
    -- let elapsed_time = tfinal = tinit

    results = scanl step (stage, telemInit, False) controls
    resultsShortCircuit = takeWhile (\(_, _, orbit_yet) -> not orbit_yet) results

    -- stageTimeIntervals = getStageTimeIntervals stages

integrateRungeKutta4 :: (Floating a, Ord a, RealFloat a) => Stage a -> Control a -> Bool -> Telemetry a -> (Stage a, Telemetry a)
integrateRungeKutta4 stage control reached_orbit telem = (stage', telem') where
    pos0 = position telem
    vel0 = velocity telem
    -- acc = acceleration telem
    dt = delta_t control

    -- mass_payload is the total mass of all the (remaining) stages,
    -- excluding the remaining prop mass of the current stage.
    mass_stages = mass_dry stage + mass_payload stage
    mass_prop_remaining = mass_prop stage
    mass_curr = mass_stages + mass_prop_remaining - 0.0

    forces0 = getForces mass_stages stage control reached_orbit (mass_prop_remaining, x pos0, y pos0, x vel0, y vel0)
    acc1 = getAcceleration forces0 mass_curr

    (_, _, forceDragMag, thrust_factor) = forces0
    prop_used = sum ( map dmdt $ engines stage) * dt * thrust_factor

    h = dt
    -- positionEulx = x pos0 + h * x vel0
    -- positionEuly = y pos0 + h * y vel0
    -- velocityEulx = x vel0 + h * x acc1
    -- velocityEuly = y vel0 + h * y acc1
    -- accelerationEul = acc1

    -- See https://en.wikipedia.org/wiki/Midpoint_method
    h1 = dt / 2.0
    pos1x = x pos0 + h1 * x vel0
    pos1y = y pos0 + h1 * y vel0
    pos1 = Coords pos1x pos1y

    vel1x = x vel0 + h1 * x acc1
    vel1y = y vel0 + h1 * y acc1
    vel1 = Coords vel1x vel1y

    mass_curr_minus_2 = mass_stages + mass_prop_remaining - (prop_used / 2.0)
    forces1 = getForces mass_stages stage control reached_orbit (mass_prop_remaining - prop_used / 2.0, x pos1, y pos1, x vel1, y vel1)
    acc2 = getAcceleration forces1 mass_curr_minus_2

    -- positionMidx = x pos0 + h * x vel0
    -- positionMidy = y pos0 + h * y vel0
    -- velocityMidx = x vel0 + h * x acc2
    -- velocityMidy = y vel0 + h * y acc2

    -- See https://en.wikipedia.org/wiki/Runge–Kutta_methods

    h2 = dt / 2.0
    pos2x = x pos0 + h2 * x vel1
    pos2y = y pos0 + h2 * y vel1
    pos2 = Coords pos2x pos2y

    vel2x = x vel0 + h2 * x acc2
    vel2y = y vel0 + h2 * y acc2
    vel2 = Coords vel2x vel2y

    -- mass_curr_minus_2 = mass_stages + mass_prop_remaining - (prop_used / 2.0)
    forces2 = getForces mass_stages stage control reached_orbit (mass_prop_remaining - prop_used / 2.0, x pos2, y pos2, x vel2, y vel2)
    acc3 = getAcceleration forces2 mass_curr_minus_2

    pos3x = x pos0 + h * x vel2
    pos3y = y pos0 + h * y vel2
    pos3 = Coords pos3x pos3y

    vel3x = x vel0 + h * x acc3
    vel3y = y vel0 + h * y acc3
    vel3 = Coords vel3x vel3y

    mass_curr_minus = mass_stages + mass_prop_remaining - prop_used
    forces3 = getForces mass_stages stage control reached_orbit (mass_prop_remaining, x pos3, y pos3, x vel3, y vel3)
    acc4 = getAcceleration forces3 mass_curr_minus

    -- Finally, perform a weighted average of the four estimates.
    -- This is essentially Simpson's rule.
    vel_rk4_x = (x vel0 + 2.0*x vel1 + 2.0*x vel2 + x vel3) / 6.0
    vel_rk4_y = (y vel0 + 2.0*y vel1 + 2.0*y vel2 + y vel3) / 6.0
    pos_rk4 = Coords{x = x pos0 + dt * vel_rk4_x, y = y pos0 + dt * vel_rk4_y}
    acc_rk4_x = (x acc1 + 2.0*x acc2 + 2.0*x acc3 + x acc4) / 6.0
    acc_rk4_y = (y acc1 + 2.0*y acc2 + 2.0*y acc3 + y acc4) / 6.0
    vel_rk4 = Coords{x = x vel0 + dt * acc_rk4_x, y = y vel0 + dt * acc_rk4_y}
    acc_rk4 = Coords{x = acc_rk4_x, y = acc_rk4_y}

    radius = magnitude (x pos_rk4) (y pos_rk4)
    altitude = radius - radius_earth
    density = barometricDensity altitude
    -- convert to pressure in units of bar
    force_tot_bar = crossSectionalArea * standard_pressure

    telem' = Telemetry {
          position = pos_rk4
        , velocity = vel_rk4
        , acceleration = acc_rk4
        , barometric_density = density
        , dynamic_pressure = forceDragMag / force_tot_bar
    }
    stage' = stage {mass_prop = mass_prop_remaining - prop_used}

-- Orbital mechanics https://en.wikipedia.org/wiki/Vis-viva_equation
visviva :: Floating a => Coords a -> Coords a -> (a, a, a)
visviva position velocity = (perihelion, apohelion, eccentricity_squared) where
    v2 = x velocity * x velocity + y velocity * y velocity
    mu = big_G * mass_earth
    semi_major_a = 1.0 / (2.0 / magnitude (x position) (y position) - v2 / mu)
    -- https:--en.wikipedia.org/wiki/Semi-major_and_semi-minor_axes
    specific_angular_momentum_h = x position * y velocity - y position * x velocity
    eccentricity_squared = 1.0 - specific_angular_momentum_h ^ 2 / (semi_major_a * mu)
    -- semi_minor_bh = semi_major_a * sqrt (eccentricity_squared - 1.0) -- hyperbola
    -- semi_minor_be = semi_major_a * sqrt (1.0 - eccentricity_squared) -- ellipse
    perihelion = semi_major_a * (1.0 - sqrt eccentricity_squared)
    apohelion = semi_major_a * (1.0 + sqrt eccentricity_squared)

phiPos :: (Floating a, Ord a) => a -> a
phiPos phi = if phi >= 0.0 then phi else phi + pi

getPhi :: (Floating a, Ord a, RealFloat a) => Telemetry a -> a
getPhi telem = phiPos $ snd $ cart2pol (x $ position telem) (y $ position telem)

getAltitude :: Floating a => Telemetry a -> a
getAltitude telem = alt where
    radius = magnitude (x $ position telem) (y $ position telem)
    alt = radius - radius_earth

getVelocity :: Floating a => Telemetry a -> a
getVelocity telem = speed where
    speed = magnitude (x $ velocity telem) (y $ velocity telem)

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs

scoreTelemetries :: (Floating a, Ord a, RealFloat a) => Bool -> [[Telemetry a]] -> [a] -> [a] -> Int -> a
scoreTelemetries orbit_other telemetries times mass_prop_remaining i = score where
    downranges = map (\telems -> last $ getDownranges (map getPhi telems) times) telemetries

    -- NOTE: The various score components will in general have different units.
    -- This is okay!
    -- NOTE: Use absolute value here instead of squaring the residual otherwise,
    -- the return-to-launch-site penalty can outweigh the dV to orbit!
    -- Only care about initial stages for rtls penalty
    rtls_penalty_distance = sum $ map (^ 2) $ dropLast downranges

    -- Only care about final stage for orbit check and delta_velocity
    telemetry = last telemetries
    lastT = last telemetry
    (perihelion, apohelion, eccentricity_squared) = visviva (position lastT) (velocity lastT)

    -- Nominal parking orbit 200km x 200km
    perihelion_nominal = 200000.0
    apohelion_nominal = 200000.0
    eccentricity_nominal = (apohelion_nominal - perihelion_nominal) / (apohelion_nominal + perihelion_nominal)

    -- Check if we failed to reach orbit
    orbit_self = perihelion > radius_earth + perihelion_nominal

    penalty_eccentricity = (10000.0 * (sqrt eccentricity_squared - eccentricity_nominal)) ^ 2
    ifactor = if i < 1000 then 0.001 else 0.1

    penalty_peri = ((perihelion - perihelion_nominal) * ifactor / 1000.0) ^ 2
    penalty_apo = ((apohelion - apohelion_nominal) * ifactor / 1000.0) ^ 2

    getSpeed telem = magnitude (x $ velocity telem) (y $ velocity telem)
    speeds = map getSpeed telemetry
    delta_velocity = maximum speeds -- This ?
    -- delta_velocity = getSpeed (last telemetry) - getSpeed (head telemetry) -- or this ?

    mass_to_orbit = last mass_prop_remaining
    rtls_penalty_mass = sum $ map abs $ dropLast mass_prop_remaining
    -- map abs ? instead, guarantee we cannot run out of propellant

    score' = mass_to_orbit + delta_velocity - 10.0 * rtls_penalty_distance - 0.1 * rtls_penalty_mass - penalty_eccentricity - penalty_peri - penalty_apo

    neg_inf = -100000000
    -- If we have not reached orbit yet,
    -- we do NOT want discontinuities in the objective function, because
    -- that will cause an 'activity cliff' and prevent the algorithm from
    -- improving failing solutions. Instead, return something that will
    -- monotonically increase as we get closer to reaching orbit, such as
    -- the downrange distance of the last stage.
    score
      | orbit_self = score'
      | orbit_other = neg_inf
      | otherwise = last downranges / 100.0
        -- Divide by 100 so this is numerically smaller than the dV
        -- required to achieve orbit.
