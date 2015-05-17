{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Utilities for "Data.Sequence".

module Penny.SeqUtil
  ( SortKey(..)
  , mapKey
  , reverseOrder
  , sortByM
  , multipleSortByM
  , mapMaybeM
  , zipWithM
  , rights

  -- * Views
  , View(..)
  , nextView
  , previousView
  , seqFromView
  , allViews

  -- ** View lenses
  , onLeft
  , onView
  , onRight
  ) where

import Control.Lens.TH
import Control.Applicative hiding (empty)
import Control.Monad (liftM)
import Data.Sequence
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Functor.Contravariant
import Data.Monoid

-- | A single sort key.
data SortKey f k a = SortKey (k -> k -> Ordering) (a -> f k)

instance Contravariant (SortKey f k) where
  contramap f (SortKey cmp get) = SortKey cmp (get . f)

mapKey
  :: Functor f
  => (k -> k')
  -> (k' -> k)
  -> SortKey f k a
  -> SortKey f k' a
mapKey fwd bak (SortKey cmp get) = SortKey cmp' get'
  where
    get' = fmap (fmap fwd) get
    cmp' x y = cmp (bak x) (bak y)

reverseOrder :: SortKey f k a -> SortKey f k a
reverseOrder (SortKey cmp get) = SortKey cmp' get
  where
    cmp' x y = case cmp x y of
      GT -> LT
      LT -> GT
      EQ -> EQ

-- | Sort a 'Seq', with effects.
sortByM
  :: Monad m
  => SortKey m k a
  -> Seq a
  -> m (Seq a)
sortByM (SortKey cmp get) sq = liftM go $ T.mapM get sq
  where
    go keys
      = fmap snd
      . S.sortBy (\x y -> cmp (fst x) (fst y))
      . S.zip keys
      $ sq

-- |
-- Sort using multiple keys.  Sorting is performed using each key in
-- turn, from left to right.
multipleSortByM
  :: (Monad m, F.Foldable c)
  => c (SortKey m k a)
  -> Seq a
  -> m (Seq a)
multipleSortByM keys sq = F.foldlM (flip sortByM) sq keys

mapMaybeM
  :: Monad m
  => (a -> m (Maybe b))
  -> Seq a
  -> m (Seq b)
mapMaybeM f sq = case viewl sq of
  EmptyL -> return empty
  x :< xs -> do
    mayB <- f x
    rest <- mapMaybeM f xs
    return $ case mayB of
      Nothing -> rest
      Just b -> b <| rest

zipWithM
  :: Monad m
  => (a -> b -> m c)
  -> Seq a
  -> Seq b
  -> m (Seq c)
zipWithM f sa sb = case viewl sa of
  EmptyL -> return empty
  a :< as -> case viewl sb of
    EmptyL -> return empty
    b :< bs -> do
      t <- f a b
      rs <- zipWithM f as bs
      return $ t <| rs


rights :: Seq (Either a b) -> Seq b
rights sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> case x of
    Left _ -> rights xs
    Right r -> r <| rights xs

data View a = View
  { _onLeft :: (Seq a)
  , _onView :: a
  , _onRight :: (Seq a)
  }

makeLenses ''View

instance Functor View where
  fmap f (View l c r) = View (fmap f l) (f c) (fmap f r)

instance F.Foldable View where
  foldr f z (View l c r) = F.foldr f (f c (F.foldr f z r)) l

instance T.Traversable View where
  sequenceA (View l c r) = View <$> T.sequenceA l <*> c <*> T.sequenceA r

nextView :: View a -> Maybe (View a)
nextView (View l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just (View (l |> c) x xs)

previousView :: View a -> Maybe (View a)
previousView (View l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just (View xs x (c <| r))

seqFromView :: View a -> Seq a
seqFromView v = (_onLeft v |> _onView v) <> _onRight v

allViews :: Seq a -> Seq (View a)
allViews = go empty
  where
    go soFar sq = case viewl sq of
      EmptyL -> empty
      x :< xs -> View soFar x xs <| go (soFar |> x) xs

