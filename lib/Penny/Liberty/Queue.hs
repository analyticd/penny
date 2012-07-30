-- | An efficient first-in, first-out queue.
module Penny.Liberty.Queue (
  Queue,
  empty,
  enqueue,
  View(Empty, Front),
  view) where

import Data.Sequence ((<|))
import qualified Data.Sequence as S

newtype Queue t = Queue (S.Seq t)
                deriving (Eq, Show)

instance Functor Queue where
  fmap f (Queue s) = Queue $ fmap f s

-- | An empty queue.
empty :: Queue t
empty = Queue S.empty

-- | The queue is first-in, first-out. For example, to enqueue a list
-- so that the items in the front of the list are also in the front of
-- the queue:
--
-- > foldl enqueue empty [1,2,3,4,5]
enqueue :: Queue t -> t -> Queue t
enqueue (Queue ts) t = Queue $ t <| ts

-- | The only way to view items in the queue.
data View t =
  Empty
  -- ^ The queue is empty

  | Front (Queue t) t
    -- ^ The queue has at least one item. This shows you the item at
    -- the front of the queue as well as the queue which remains after
    -- the front item is removed. Items are enqueued at the back of
    -- the queue and the view shows you the front of the queue.
  deriving Show

-- | See which items are in the front of the queue. Items are enqueued
-- at the back of the queue and are viewed at the front.
view :: Queue t -> View t
view (Queue ts) = case S.viewr ts of
  S.EmptyR -> Empty
  rs S.:> t -> Front (Queue rs) t

