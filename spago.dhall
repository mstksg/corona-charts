{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "halogen"
  , "apexcharts"
  , "profunctor-lenses"
  , "affjax"
  , "parseint"
  , "sequence"
  , "css"
  , "halogen-css"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
