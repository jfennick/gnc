module Types where

data Engine a = Engine
    { dmdt :: a -- kg/s
    , velocity_exhaust :: a -- m/s, in rocket frame
    , diameter :: a -- m
    , mass :: a -- kg
    }
    deriving (Eq, Show)

data Coords a = Coords
    { x :: a
    , y :: a
    }
    deriving (Eq, Show)

data Telemetry a = Telemetry
    { position :: Coords a
    , velocity :: Coords a
    , acceleration :: Coords a
    , barometric_density :: a
    , dynamic_pressure :: a
    -- , mass_prop_used :: a
    }
    deriving (Eq, Show)

data Control a = Control
    -- t1 < t2
    { t1 :: a
    , t2 :: a
    -- In polar coordinates, this is the angle measured from vertical / radial
    , force_phi :: a
    , force_mag :: a -- = 1.0 i.e. the throttle
    -- , fixed :: Bool -- default False; used to exclude specific controls from being optimized.
    }
    deriving (Eq, Show)

data Stage a = Stage
    { mass_dry :: a -- kg
    , mass_payload :: a -- kg
    , mass_prop :: a -- kg
    , engines :: [Engine a]
    , burn_time :: a -- sec
    }
    deriving (Eq, Show)
