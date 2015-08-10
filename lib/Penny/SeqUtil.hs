{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Utilities for "Data.Sequence".

module Penny.SeqUtil
  ( mapMaybeM
  , zipWithM
  , lefts
  , rights
  , partitionEithers
  , filterM

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
import Data.Sequence
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid

filterM
  :: Monad m
  => (a -> m Bool)
  -> Seq a
  -> m (Seq a)
filterM pd sq = case viewl sq of
  EmptyL -> return empty
  x :< xs -> do
    b <- pd x
    rest <- filterM pd xs
    return $ if b then (x <| rest) else rest

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

lefts :: Seq (Either a b) -> Seq a
lefts sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> case x of
    Right _ -> lefts xs
    Left l -> l <| lefts xs


rights :: Seq (Either a b) -> Seq b
rights sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> case x of
    Left _ -> rights xs
    Right r -> r <| rights xs

partitionEithers
  :: Seq (Either a b)
  -> (Seq a, Seq b)
partitionEithers = F.foldl' f (empty, empty)
  where
    f (l, r) ei = case ei of
      Left a -> (l |> a, r)
      Right a -> (l, r |> a)

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

