module Test.Main
  ( main
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List as List
import Erl.Data.Queue (Queue)
import Erl.Data.Queue as Queue
import Erl.Data.Tuple (tuple2)
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual, assertFalse, assertTrue)
import Test.QuickCheck ((===))
import Test.QuickCheck.Helpers (Properties(..), property)

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "queue operations" do
      test "put" do
        property "`singleton` produces the same as `fromFoldable` & `put`" \(x :: Int) -> do
          Properties
            [ Queue.singleton x === (Queue.empty # Queue.put x)
            , Queue.singleton x === Queue.fromFoldable [ x ]
            ]

      test "put, get, getBack & putFront" do
        property "`get` gives us the first element inserted" \(x :: Int) (y :: Int) (z :: Int) -> do
          let
            q = Queue.empty # Queue.put x # Queue.put y # Queue.put z
            afterGet = Queue.get q
          (Just { item: x, queue: Queue.fromFoldable [ y, z ] }) === afterGet

        property "`getBack` gives us the last element inserted" \(x :: Int) (y :: Int) (z :: Int) ->
          do
            let
              q = Queue.empty # Queue.put x # Queue.put y # Queue.put z
              afterGetBack = Queue.getBack q
            (Just { item: z, queue: Queue.fromFoldable [ x, y ] }) === afterGetBack

        property "`putFront`" \(x :: Int) (y :: Int) (z :: Int) (inFront :: Int) -> do
          let
            q = Queue.empty # Queue.put x # Queue.put y # Queue.put z
            afterPutFront = Queue.putFront inFront q
          Properties
            [ (Queue.fromFoldable [ inFront, x, y, z ]) === afterPutFront
            , (Queue.get afterPutFront) ===
                (Just { item: inFront, queue: Queue.fromFoldable [ x, y, z ] })
            ]

      test "get on empty returns `Nothing`" do
        let
          q = Queue.empty :: Queue Int
          afterGet = Queue.get q
        assertEqual { expected: Nothing, actual: afterGet }

      test "peek leaves the queue unchanged" do
        property "" \(x :: Int) (xs :: Queue Int) -> do
          let
            q = Queue.singleton x <> xs
            afterPeek = Queue.peek q
          Just x === afterPeek

      test "`Semigroup` & `Monoid`" do
        property "`q1 <> q2` yields the same as `(xs <> ys) # Queue.fromFoldable`"
          \(xs :: Array Int) (ys :: Array Int) -> do
            let q = Queue.fromFoldable xs <> Queue.fromFoldable ys
            ((xs <> ys) # Queue.fromFoldable) === q

        property "Right identity" \(q :: Queue Int) -> do
          (q <> Queue.empty) === q

        property "Left identity" \(q :: Queue Int) -> do
          (Queue.empty <> q) === q

      test "`split`" do
        property "`split (length xs) (xs <> ys)` gives us back `xs` & `ys`"
          \(xs :: Queue Int) (ys :: Queue Int) -> do
            let
              q = xs <> ys
              splitResult = Queue.split (Queue.length xs) q
              expectedTuple = tuple2 xs ys
            Just expectedTuple === splitResult

      test "`isEmpty`" do
        let q = Queue.empty :: Queue Int
        q # Queue.isEmpty # assertTrue
        q # Queue.put 42 # Queue.isEmpty # assertFalse

        property "getting all items from a queue yields an empty queue" \(xs :: Queue Int) -> do
          let
            takeItem q' _v = q' # Queue.get # case _ of
              Just { queue } -> queue
              Nothing -> q'
          if Queue.length xs /= 0 then
            (xs # (\q' -> foldl takeItem q' q') # Queue.isEmpty) === true
          else
            Queue.isEmpty xs === true

      test "`reverse`" do
        let q = Queue.fromFoldable [ 1, 2, 3, 4, 5 ]
        q
          # Queue.reverse
          # Queue.toList
          # \actual -> assertEqual { expected: List.fromFoldable [ 5, 4, 3, 2, 1 ], actual }

        property "`reverse` works the same as for `List`" \(xs :: Array Int) ->
          xs
            # Queue.fromFoldable
            # Queue.reverse
            # Queue.toList
            # (\result -> result === (xs # List.fromFoldable # List.reverse))

        property "`reverse >>> reverse` gives us the same queue back" \(xs :: Queue Int) ->
          xs
            # Queue.reverse
            # Queue.reverse
            # (\result -> result === xs)

      test "`map`" do
        property "`map` works the same as for `List`" \(xs :: Array Int) ->
          xs
            # Queue.fromFoldable
            # map (_ + 1)
            # Queue.toList
            # (\result -> result === (xs # List.fromFoldable # map (_ + 1)))

      test "`filter`" do
        property "`filter` works the same as for `List`" \(xs :: Array Int) ->
          xs
            # Queue.fromFoldable
            # Queue.filter (_ > 0)
            # Queue.toList
            # (\result -> result === (xs # List.fromFoldable # List.filter (_ > 0)))

