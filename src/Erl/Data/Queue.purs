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
  , putBack
  , getBack
  ) where

import Control.Category ((>>>))
import Data.Eq (class Eq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Function ((#))
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Erl.Data.List (List)
import Erl.Data.List as List

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
  eq queue1 queue2 = eq_ queue1 queue2

fromFoldable :: forall f a. Foldable f => Eq a => f a -> Queue a
fromFoldable = List.fromFoldable >>> fromList_

-- | An empty queue
empty :: forall a. Queue a
empty = empty_

-- | Test if a queue is empty
isEmpty :: forall a. Queue a -> Boolean
isEmpty s = isEmpty_ s

-- | Create a queue with one element
singleton :: forall a. a -> Queue a
singleton a = singleton_ a

-- | Get an element from the queue
get :: forall a. Queue a -> Maybe (OutResult a)
get queue = out_ queue

-- | Put an element in the queue
put :: forall a. a -> Queue a -> Queue a
put a queue = in_ a queue

-- | Put an element at the back of the queue
putBack :: forall a. a -> Queue a -> Queue a
putBack a queue = in_r_ a queue

-- | Get an element from the back of the queue
getBack :: forall a. Queue a -> Maybe (OutResult a)
getBack queue = out_r_ queue

-- | Get the length of a queue
length :: forall a. Queue a -> Int
length queue = len_ queue

-- | Convert a queue to a list
toList :: forall a. Queue a -> List a
toList queue = toList_ queue

-- | Reverse a queue
reverse :: forall a. Queue a -> Queue a
reverse queue = reverse_ queue

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
