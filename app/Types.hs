{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Dhall ( Generic, FromDhall, ToDhall )

data Engine a = Engine
    { dmdt :: a -- kg/s
    , velocity_exhaust :: a -- m/s, in rocket frame
    , diameter :: a -- m
    , mass :: a -- kg
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)

data Coords a = Coords
    { x :: a
    , y :: a
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)

data Telemetry a = Telemetry
    { position :: Coords a
    , velocity :: Coords a
    , acceleration :: Coords a
    , barometric_density :: a
    , dynamic_pressure :: a
    -- , mass_prop_used :: a
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)

data Control a = Control
    { delta_t :: a -- > 0
    -- In polar coordinates, this is the angle measured from vertical / radial
    , force_phi :: a
    , force_mag :: a -- = 1.0 i.e. the throttle
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)

data Stage a = Stage
    { mass_dry :: a -- kg
    , mass_payload :: a -- kg
    , mass_prop :: a -- kg
    , engines :: [Engine a]
    , burn_time :: a -- sec
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)

data Config a = Config
    { dtMax :: a
    , accLimit :: a
    , stages :: [Stage a]
    , controls :: [Control a]
    }
    deriving (Eq, Show, Generic, ToDhall, FromDhall)
