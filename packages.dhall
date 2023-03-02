let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.15.3-20220629/packages.dhall sha256:48ee9f3558c00e234eae6b8f23b4b8b66eb9715c7f2154864e1e425042a0723b

let erl-quickcheck-helpers =
      https://raw.githubusercontent.com/quanterall/purescript-erl-quickcheck-helpers/v0.0.4/spago.dhall
        sha256:df6abef567d04b64cef1dd714d1e872d7e7800cf89a9a2184fbb35ccb3c65468

let overrides =
      { erl-quickcheck-helpers =
        { repo =
            "https://github.com/quanterall/purescript-erl-quickcheck-helpers.git"
        , version = "v0.0.4"
        , dependencies = erl-quickcheck-helpers.dependencies
        }
      }

in  upstream // overrides
