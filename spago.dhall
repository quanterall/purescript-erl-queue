{ name = "erl-sets"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "erl-lists"
  , "erl-test-eunit"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
