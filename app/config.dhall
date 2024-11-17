let types = ./types.dhall
in
{ dtMax = 0.5
, accLimit = 4.0
, stages = [] : List types.Stage
, controls = [] : List types.Control} : types.Config