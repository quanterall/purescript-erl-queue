module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

main :: Effect Unit
main = Console.log "Running tests..."
