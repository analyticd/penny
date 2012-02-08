{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.Zinc.Expressions.Queue (
  Back,
  empty,
  enqueue,
  Front,
  front,
  View(Empty, (:<)),
  view) where

import Data.Sequence ((<|))
import qualified Data.Sequence as S
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

newtype Back t = Back (S.Seq t)
            deriving (Show, Functor, Foldable, Traversable)

empty :: Back t
empty = Back S.empty

enqueue :: t -> Back t -> Back t
enqueue t (Back ts) = Back $ t <| ts

newtype Front t = Front (S.Seq t)
                deriving (Show, Functor, Foldable, Traversable)

front :: Back t -> Front t
front (Back ts) = Front ts

data View t =
  Empty
  | t :< Front t
  deriving Show

view :: Front t -> View t
view (Front ts) = case S.viewl ts of
  S.EmptyL -> Empty
  t S.:< rs -> t :< Front rs

