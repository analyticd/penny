module Penny.Zinc.Expressions.Queue (
  Queue,
  empty,
  enqueue,
  View(Empty, (:>)),
  view) where

import Data.Sequence ((<|))
import qualified Data.Sequence as S
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

newtype Queue t = Queue (S.Seq t)
                deriving Show

empty :: Queue t
empty = Queue S.empty

enqueue :: t -> Queue t -> Queue t
enqueue t (Queue ts) = Queue $ t <| ts

data View t =
  Empty
  | Queue t :> t
  deriving Show

view :: Queue t -> View t
view (Queue ts) = case S.viewr ts of
  S.EmptyR -> Empty
  rs S.:> t -> Queue rs :> t

