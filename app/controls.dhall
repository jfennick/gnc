let types = ./types.dhall

let Controls = types.Controls.Controls
let PieceWise = types.Controls.PieceWise
let NullStaging = types.Controls.NullStaging

let negpi2 = -1.5707963267948966 -- -pi/2
let pi2 = 1.5707963267948966 -- pi/2
let pi4 = 0.7853981633974483 -- pi/4
let pi35 = 0.8975979010256552 -- pi/3.5
let pi6 = 0.5235987755982989 -- pi/6
let pi2pi12 = 1.3089969389957472 -- pi/2.0-pi/12

let uptime = 10.0
let up = Controls [{delta_t = uptime, force_phi = 0.0, force_mag = 1.0 }]
let over = Controls [{delta_t = 1000.0, force_phi = pi2, force_mag = 1.0}]
let upover = [[up, over]]

let numturns1 = Natural/toInteger 20
let numturns2 = Natural/toInteger 10
let stagingTime = 5.0

let gravityTurn1 = \(bt : Double) -> PieceWise { init_angle = 0.0, final_angle = pi35, numturns = numturns1, turntime = bt}
let gravityTurn1a = \(bt : Double) -> PieceWise { init_angle = 0.0, final_angle = pi6, numturns = numturns1, turntime = bt}
let gravityTurn1b = \(bt : Double) -> PieceWise { init_angle = pi6, final_angle = pi4, numturns = numturns1, turntime = bt}
let stagingDelay = NullStaging stagingTime : types.Controls

let gravityTurn2 = \(bt : Double) -> PieceWise { init_angle = pi4, final_angle = pi2pi12, numturns = numturns2, turntime = bt}

let over12 = Controls [{delta_t = 140.0, force_phi = pi2pi12, force_mag = 1.0 }]
let over12PW = PieceWise { init_angle = pi2, final_angle = pi2, numturns = Natural/toInteger 10, turntime = 137.0}

let center13 = 0.393939394 -- 13/33
let center3 = 0.090909091 -- 3/33

let boostback = Controls [{delta_t = 45.5, force_phi = negpi2, force_mag = center13 }]
-- let boostbackPW = PieceWise { init_angle = negpi2, final_angle = negpi2, numturns = Natural/toInteger 10, turntime = 39.5}

let coastPhase1 = NullStaging 180.0
let coastPhase2 = NullStaging 450.0 -- 4500.0

let landingBurn13 = Controls [{delta_t = 6.5, force_phi = 0.0, force_mag = center13 }]
let landingBurn3 = Controls [{delta_t = 20.0, force_phi = 0.0, force_mag = center3 }]

let controls =
    [ [up, gravityTurn1 120.0, stagingDelay, boostback, coastPhase1, landingBurn13, landingBurn3]
    , [gravityTurn2 140.0, over12] -- coastPhase2
    ]

in
controls