{ name = "erl-queue"
, dependencies =
  [ "assert"
  , "effect"
  , "erl-lists"
  , "erl-test-eunit"
  , "erl-tuples"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
