{-# LANGUAGE DeriveGeneric #-}

module Penny.Lincoln.Family.Family where

import Control.Applicative ((<$>), (<*>), pure, Applicative)
import qualified Data.Binary as B
import qualified Data.List as L
import Data.Traversable (traverse)
import GHC.Generics (Generic)

-- | A Family has one parent (ah, the anomie, sorry) and at least two
-- children.
data Family p c =
  Family { parent :: p
         , child1 :: c
         , child2 :: c
         , children :: [c] }
  deriving (Eq, Show, Generic)

instance (B.Binary p, B.Binary c) => B.Binary (Family p c)

-- | Maps over all children, in order starting with child
-- 1, then child 2, then the children in the list from left to right.
mapChildrenA ::
  Applicative m
  => (a -> m b)
  -> Family p a
  -> m (Family p b)
mapChildrenA f (Family p c1 c2 cs) =
  Family p <$> f c1 <*> f c2 <*> traverse f cs


-- | Maps over all children.
mapChildren ::
  (a -> b)
  -> Family p a
  -> Family p b
mapChildren f (Family p c1 c2 cs) =
  Family p (f c1) (f c2) (map f cs)


-- | Maps over the parent in an Applicative.
mapParentA ::
  Applicative m
  => (a -> m b)
  -> Family a c
  -> m (Family b c)
mapParentA f (Family p c1 c2 cs) =
  Family <$> f p <*> pure c1 <*> pure c2 <*> pure cs


-- | Maps over the parent.
mapParent :: (a -> b) -> Family a c -> Family b c
mapParent f (Family p c1 c2 cs) = Family (f p) c1 c2 cs


-- | Finds the first child matching a predicate.
find :: (p -> c -> Bool) -> Family p c -> Maybe c
find f (Family p c1 c2 cs)
  | f p c1 = Just c1
  | f p c2 = Just c2
  | otherwise = L.find (f p) cs

-- | Filters the children. Fails if there are not at least two
-- children after filtering. Retains the original order of the
-- children (after removing the children you don't want.)
filterChildren :: (a -> Bool) -> Family p a -> Maybe (Family p a)
filterChildren f (Family p c1 c2 cs) =
  case (f c1, f c2) of
    (True, True) -> Just (Family p c1 c2 (filter f cs))
    (True, False) ->
      case filter f cs of
        [] -> Nothing
        x:xs -> Just (Family p c1 x xs)
    (False, True) ->
      case filter f cs of
        [] -> Nothing
        x:xs -> Just (Family p c2 x xs)
    (False, False) ->
      case filter f cs of
        x1:x2:xs -> Just (Family p x1 x2 xs)
        _ -> Nothing
