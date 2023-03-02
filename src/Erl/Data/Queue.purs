-- | A FIFO queue as represented by the `queue` Erlang module.
module Erl.Data.Queue
  ( Queue
  , fromFoldable
  , empty
  , isEmpty
  , singleton
  , get
  , put
  , length
  , reverse
  , toList
  , putFront
  , getBack
  , peek
  , split
  , toUnfoldable
  , filter
  ) where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe)
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Tuple (Tuple2)
import Test.QuickCheck (class Arbitrary, arbitrary)

type OutResult a = { item :: a, queue :: Queue a }

-- | An Erlang queue as created by the `queue` module.
foreign import data Queue :: Type -> Type

instance semigroupQueue :: Semigroup (Queue a) where
  append queue1 queue2 = join_ queue1 queue2

instance monoidQueue :: Monoid (Queue a) where
  mempty = empty

instance foldableQueue :: Foldable Queue where
  foldl f acc queue = queue # toList # foldl f acc
  foldr f acc queue = queue # toList # foldr f acc
  foldMap f queue = queue # toList # foldMap f

instance eqQueue :: Eq a => Eq (Queue a) where
  eq queue1 queue2 = toList queue1 == toList queue2

instance showQueue :: Show a => Show (Queue a) where
  show queue = "fromFoldable " <> show (toList queue)

instance functorQueue :: Functor Queue where
  map f queue = map_ f queue

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = do
    (xs :: Array a) <- arbitrary
    xs # fromFoldable # pure

fromFoldable :: forall f a. Foldable f => f a -> Queue a
fromFoldable = List.fromFoldable >>> fromList_

-- It's unclear to me whether this implementation is better or worse:
--
--   unfoldr (get >>> map (\{ item, queue } -> item /\ queue))
--
toUnfoldable :: forall f a. Unfoldable f => Queue a -> f a
toUnfoldable = toList >>> List.toUnfoldable

-- | Creates a new empty queue.
empty :: forall a. Queue a
empty = empty_

-- | Tests whether a queue is empty.
isEmpty :: forall a. Queue a -> Boolean
isEmpty s = isEmpty_ s

-- | Creates a new queue with only the supplied element.
singleton :: forall a. a -> Queue a
singleton a = singleton_ a

-- | Gets an element from the queue if at least one exists and returns the queue without it.
get :: forall a. Queue a -> Maybe (OutResult a)
get queue = out_ queue

-- | Returns a new queue with the supplied element added to the back.
put :: forall a. a -> Queue a -> Queue a
put a queue = in_ a queue

-- | Returns a new queue with the supplied element added to the front.
putFront :: forall a. a -> Queue a -> Queue a
putFront a queue = in_r_ a queue

-- | Gets an element from the back of a queue.
getBack :: forall a. Queue a -> Maybe (OutResult a)
getBack queue = out_r_ queue

-- | Returns the number of elements in a queue.
length :: forall a. Queue a -> Int
length queue = len_ queue

-- | Returns the elements of a queue as a list.
toList :: forall a. Queue a -> List a
toList queue = toList_ queue

-- | Splits a queue into two queues at the specified index.
split :: forall a. Int -> Queue a -> Maybe (Tuple2 (Queue a) (Queue a))
split n queue = split_ n queue

-- | Reverses a queue.
reverse :: forall a. Queue a -> Queue a
reverse queue = reverse_ queue

-- | Returns the first element of a queue if at least one exists.
peek :: forall a. Queue a -> Maybe a
peek queue = peek_ queue

-- | Filters a queue using the supplied predicate, returning a new queue containing only the
-- | elements that satisfy the predicate.
filter :: forall a. (a -> Boolean) -> Queue a -> Queue a
filter f queue = filter_ f queue

foreign import fromList_ :: forall a. List a -> Queue a

foreign import empty_ :: forall a. Queue a

foreign import isEmpty_ :: forall a. Queue a -> Boolean

foreign import singleton_ :: forall a. a -> Queue a

foreign import out_ :: forall a. Queue a -> Maybe (OutResult a)

foreign import in_ :: forall a. a -> Queue a -> Queue a

foreign import len_ :: forall a. Queue a -> Int

foreign import join_ :: forall a. Queue a -> Queue a -> Queue a

foreign import fold_ :: forall a b. (b -> a -> b) -> b -> Queue a -> b

foreign import reverse_ :: forall a. Queue a -> Queue a

foreign import toList_ :: forall a. Queue a -> List a

foreign import in_r_ :: forall a. a -> Queue a -> Queue a

foreign import out_r_ :: forall a. Queue a -> Maybe (OutResult a)

foreign import eq_ :: forall a. Queue a -> Queue a -> Boolean

foreign import peek_ :: forall a. Queue a -> Maybe a

foreign import split_ :: forall a. Int -> Queue a -> Maybe (Tuple2 (Queue a) (Queue a))

foreign import map_ :: forall a b. (a -> b) -> Queue a -> Queue b

foreign import filter_ :: forall a. (a -> Boolean) -> Queue a -> Queue a
