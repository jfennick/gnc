let Engine =
    { dmdt : Double
    , velocity_exhaust : Double
    , diameter : Double
    , mass : Double
    }

let Coords =
    { x : Double
    , y : Double
    }

let Telemetry =
    { position : Coords
    , velocity : Coords
    , acceleration : Coords
    , barometric_density : Double
    , dynamic_pressure : Double
    }

let Control =
    { delta_t : Double
    -- In polar coordinates, this is the angle measured from vertical / radial
    , force_phi : Double
    , force_mag : Double
    }

let Stage =
    { mass_dry : Double
    , mass_payload : Double
    , mass_prop : Double
    , engines : List Engine
    }

let PieceWiseTurn =
    { init_angle : Double
    , final_angle : Double
    , numturns : Integer
    , turntime : Double
    }

let Controls =
    < Controls : List Control
    | PieceWise : PieceWiseTurn
    | NullStaging : Double
    | NullCoast : Double
    >

let Config =
    { dtMax : Double
    , accLimit : Double
    , stages : List Stage
    , controls : List (List Controls)
    }

in
{ Engine
, Coords
, Telemetry
, Control
, Stage
, Config
, PieceWiseTurn
, Controls
}