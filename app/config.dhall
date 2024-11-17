let types = ./types.dhall
let rockets = ./rockets.dhall
let controls = ./controls.dhall
in
{ dtMax = 0.5
, accLimit = 4.0
, stages = [rockets.superheavy_V1, rockets.starship_V1] : List types.Stage
, controls = controls : List (List types.Controls)} : types.Config
