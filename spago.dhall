{ name = "erl-queue"
, dependencies =
  [ "arrays"
  , "assert"
  , "effect"
  , "erl-lists"
  , "erl-test-eunit"
  , "erl-tuples"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "quickcheck"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
