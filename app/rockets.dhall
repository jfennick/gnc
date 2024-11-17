let r2 =
    { dmdt = 650.0
    , velocity_exhaust = 3200.0
    , diameter = 1.3
    , mass = 1600.0
    }

let r2vac =
    { dmdt = 650.0
    , velocity_exhaust = 3560.0
    , diameter = 2.4
    , mass = 1600.0
    }

let superheavy_V1 =
    { mass_dry = 200000.0
    , mass_payload = 0.0
    , mass_prop = 3400000.0
    -- 33
    , engines = [r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2]
    }

let starship_V1 =
    { mass_dry = 100000.0
    , mass_payload = 40000.0
    , mass_prop = 1200000.0
    , engines = [r2, r2 ,r2, r2vac, r2vac, r2vac]
    }

-- CF6-50 Turbofan
let cf6_mass = 3960.0 -- kg
let cf6_TWR = 6.01 -- unitless
let cf6_SFC = 0.0105 -- (kg/s)/kN
let cf6_thrust = 233474.076 -- (cf6_mass * littleg) * cf6_TWR -- Newton
let cf6_fuel_rate = 2.451477798 -- (cf6_thrust / 1000.0) * cf6_SFC
let cf6 =
    { dmdt = cf6_fuel_rate
    , velocity_exhaust = 95000.0
    , diameter = 2.19
    , mass = cf6_mass
    }

-- i.e. 12 hexagonal rings of turbofans, with the central turbofan + inner 3 rings removed.
-- This should give about 10% more thrust than superheavy, and about 5X more dry mass. (1425 Tons...)
-- Note that this dry mass estimate does not include any of the structural mass
-- necessary to actually connect the turbofans together and to the rocket, but
-- it also doesn't include any mass savings you would get by designing an
-- optimized turbofan specifically for this application.
let numFans = Natural/toInteger 360 -- = 397 - 37 = circlePackingNumber 12 - circlePackingNumber 4
let fans = [cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6, cf6]

let turbostage =
    { mass_dry = 1425600 -- numFans * cf6_mass
    , mass_payload = 0.0
    , mass_prop = 50000.0
    , engines = fans
    }

let superheavyTurbo =
    { mass_dry = 200000.0
    , mass_payload = 0.0
    , mass_prop = 3350000 -- mass_prop superheavy - mass_prop turbostage
    , engines = fans
    }

-- In thrust we trust!
let superheavyA =
    { mass_dry = 200000.0
    , mass_payload = 0.0
    , mass_prop = 2200000.0
    -- 33 + 25
    , engines = [r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2]
    }

let superheavyB =
    { mass_dry = 100000.0
    , mass_payload = 0.0
    , mass_prop = 1200000.0
    -- 30
    , engines = [r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2]
    }

let superheavyATurbo =
    { mass_dry = 200000.0
    , mass_payload = 0.0
    , mass_prop = 2150000 -- mass_prop superheavyA - mass_prop turbostage
    -- 33 + 35
    , engines = [r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2, r2]
    }

in
{ r2
, r2vac
, cf6
, superheavy_V1
, starship_V1
, turbostage
, superheavyTurbo
, superheavyA
, superheavyB
, superheavyATurbo
}