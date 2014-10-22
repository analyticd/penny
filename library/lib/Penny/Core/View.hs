module Penny.Core.View where

import Data.Sequence
import qualified Penny.Core.Ent as Ent

data T a = T
  { left :: Seq (Ent.T a)
  , current :: Ent.T a
  , right :: Seq (Ent.T a)
  } deriving (Eq, Ord, Show)

moveLeft :: T a -> Maybe (T a)
moveLeft (T l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just (T xs x (c <| r))

moveRight :: T a -> Maybe (T a)
moveRight (T l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just (T (l |> c) x xs)

allViews :: Seq (Ent.T a) -> Seq (T a)
allViews = go empty
  where
    go soFar sq = case viewl sq of
      EmptyL -> empty
      x :< xs -> T soFar x xs <| go (soFar |> x) xs
