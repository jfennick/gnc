module Types where

data Engine = Engine
    { dmdt :: Double -- kg/s
    , velocity_exhaust :: Double -- m/s, in rocket frame
    , diameter :: Double -- m
    , mass :: Double -- kg
    }
    deriving (Eq, Show)

data Coords = Coords
    { x :: Double
    , y :: Double
    }
    deriving (Eq, Show)

data Telemetry = Telemetry
    { position :: Coords
    , velocity :: Coords
    , acceleration :: Coords
    , barometric_density :: Double
    , dynamic_pressure :: Double
    -- , mass_prop_used :: Double
    }
    deriving (Eq, Show)

data Control = Control
    -- t1 < t2
    { t1 :: Double
    , t2 :: Double
    -- In polar coordinates, this is the angle measured from vertical / radial
    , force_phi :: Double
    , force_mag :: Double -- = 1.0 i.e. the throttle
    , fixed :: Bool -- default False; used to exclude specific controls from being optimized.
    }
    deriving (Eq, Show)

data Stage = Stage
    { mass_dry :: Double -- kg
    , mass_payload :: Double -- kg
    , mass_prop :: Double -- kg
    , engines :: [Engine]
    , burn_time :: Double -- sec
    }
    deriving (Eq, Show)
