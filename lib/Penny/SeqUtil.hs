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
  , intersperse
  , singleSeq

  -- * Slices
  , Slice(..)
  , nextSlice
  , previousSlice
  , seqFromSlice
  , allSlices

  -- ** Slice lenses
  , onLeft
  , onSlice
  , onRight
  ) where

import Control.Lens.TH
import Control.Lens.Cons (uncons)
import Data.Sequence
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Monad (join)

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

intersperse
  :: a
  -> Seq a
  -> Seq a
intersperse between sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> x <| withBetweens
    where
      withBetweens = join . fmap (\a -> between <| a <| empty) $ xs

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

singleSeq :: Seq a -> Maybe a
singleSeq sq = case uncons sq of
  Nothing -> Nothing
  Just (x, xs) -> case uncons xs of
    Nothing -> Just x
    Just _ -> Nothing

data Slice a = Slice
  { _onLeft :: (Seq a)
  , _onSlice :: a
  , _onRight :: (Seq a)
  }

makeLenses ''Slice

instance Functor Slice where
  fmap f (Slice l c r) = Slice (fmap f l) (f c) (fmap f r)

instance F.Foldable Slice where
  foldr f z (Slice l c r) = F.foldr f (f c (F.foldr f z r)) l

instance T.Traversable Slice where
  sequenceA (Slice l c r) = Slice <$> T.sequenceA l <*> c <*> T.sequenceA r

nextSlice :: Slice a -> Maybe (Slice a)
nextSlice (Slice l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just (Slice (l |> c) x xs)

previousSlice :: Slice a -> Maybe (Slice a)
previousSlice (Slice l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just (Slice xs x (c <| r))

seqFromSlice :: Slice a -> Seq a
seqFromSlice v = (_onLeft v |> _onSlice v) <> _onRight v

allSlices :: Seq a -> Seq (Slice a)
allSlices = go empty
  where
    go soFar sq = case viewl sq of
      EmptyL -> empty
      x :< xs -> Slice soFar x xs <| go (soFar |> x) xs

