module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.Queue (Queue)
import Erl.Data.Queue as Queue
import Erl.Data.Tuple (tuple2)
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "queue operations" do
      test "put" do
        let q = Queue.empty # Queue.put 42 # Queue.put 1337
        assertEqual { expected: 42 : 1337 : nil, actual: Queue.toList q }
        assertEqual { expected: Queue.fromFoldable [ 42, 1337 ], actual: q }

      test "put, get, getBack & putFront" do
        let
          q = Queue.empty # Queue.put 42 # Queue.put 1337 # Queue.put 0
          afterGet = Queue.get q
          afterGetBack = Queue.getBack q
          afterPutFront = Queue.putFront 5 q
        assertEqual
          { expected: Just { item: 42, queue: Queue.fromFoldable [ 1337, 0 ] }
          , actual: afterGet
          }
        assertEqual
          { expected: Just { item: 0, queue: Queue.fromFoldable [ 42, 1337 ] }
          , actual: afterGetBack
          }
        assertEqual
          { expected: Queue.fromFoldable [ 5, 42, 1337, 0 ]
          , actual: afterPutFront
          }

      test "get on empty returns `Nothing`" do
        let
          q = Queue.empty :: Queue Int
          afterGet = Queue.get q
        assertEqual { expected: Nothing, actual: afterGet }

      test "peek leaves the queue unchanged" do
        let
          q = 42 # Queue.singleton # Queue.put 1337 # Queue.put 0
          afterPeek = Queue.peek q
        assertEqual { expected: Just 42, actual: afterPeek }
        assertEqual { expected: Queue.fromFoldable [ 42, 1337, 0 ], actual: q }

      test "split creates two queues from one" do
        let
          q = Queue.fromFoldable [ 1, 2, 3, 4, 5 ]
          splitResult = Queue.split 3 q
          expectedTuple = tuple2 (Queue.fromFoldable [ 1, 2, 3 ]) (Queue.fromFoldable [ 4, 5 ])
        assertEqual { expected: Just expectedTuple, actual: splitResult }

