module Penny.Liberty.Queue (
  Queue,
  empty,
  enqueue,
  View(Empty, Front),
  view) where

import Data.Sequence ((<|))
import qualified Data.Sequence as S

newtype Queue t = Queue (S.Seq t)
                deriving (Eq, Read, Show, Ord)

instance Functor Queue where
  fmap f (Queue s) = Queue $ fmap f s

empty :: Queue t
empty = Queue S.empty

enqueue :: t -> Queue t -> Queue t
enqueue t (Queue ts) = Queue $ t <| ts

data View t =
  Empty
  | Front (Queue t) t
  deriving Show

view :: Queue t -> View t
view (Queue ts) = case S.viewr ts of
  S.EmptyR -> Empty
  rs S.:> t -> Front (Queue rs) t

