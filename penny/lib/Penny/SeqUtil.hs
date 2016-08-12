{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Utilities for "Data.Sequence".

module Penny.SeqUtil
  ( mapMaybe
  , mapMaybeM
  , zipWithM
  , lefts
  , rights
  , partitionEithers
  , yank
  , yankSt
  , Groups(..)
  , groupEithers
  , filterM
  , intersperse
  , singleSeq
  , convertHead
  , catMaybes
  , isSingleton

  -- * Slices
  , Slice(..)
  , nextSlice
  , previousSlice
  , seqFromSlice
  , allSlices
  , prettySlice

  -- ** Slice lenses
  , onLeft
  , onSlice
  , onRight
  ) where

import Control.Lens.TH
import Control.Lens.Cons (uncons)
import Control.Monad (join)
import qualified Control.Monad.State as St
import Data.Sequence
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Traversable as T
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

import Penny.Pretty

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

mapMaybe
  :: (a -> Maybe b)
  -> Seq a
  -> Seq b
mapMaybe f sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> case f x of
    Nothing -> mapMaybe f xs
    Just b -> b <| mapMaybe f xs

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

-- | Finds the first item on which the given function returns Just.
-- If found, returns the resulting Just, and the sequence with that
-- single item removed.  Otherwise, returns Nothing.
yank :: (a -> Maybe b) -> Seq a -> Maybe (b, Seq a)
yank f = go empty
  where
    go acc sq = case viewl sq of
      EmptyL -> Nothing
      x :< xs -> case f x of
        Nothing -> go (acc |> x) xs
        Just a -> Just (a, acc <> xs)

-- | Like 'yank' but runs in a state monad.
yankSt :: St.MonadState (Seq a) m => (a -> Maybe b) -> m (Maybe b)
yankSt f = do
  st <- St.get
  case yank f st of
    Nothing -> return Nothing
    Just (r, st') -> do
      St.put st'
      return (Just r)

data Groups a b = Groups
  { _leaders :: Seq b
  , _middle :: Seq (NonEmptySeq a, NonEmptySeq b)
  , _trailers :: Seq a
  }

groupEithers
  :: Seq (Either a b)
  -> Groups a b
groupEithers sq = Groups l m r
  where
    (l, middleAndTrail) = go empty sq
      where
        go acc sq = case viewl sq of
          EmptyL -> (acc, empty)
          x :< xs -> case x of
            Right a -> go (acc |> a) xs
            Left _ -> (acc, sq)
    (middle, mayLast) = foldl appendMiddle (empty, Nothing) middleAndTrail
    (m, r) = case mayLast of
      Nothing -> (middle, empty)
      Just (as, bs) -> case NE.seqToNonEmptySeq bs of
        Nothing -> (middle, NE.nonEmptySeqToSeq as)
        Just bss -> (middle |> (as, bss), empty)

    appendMiddle
      :: (Seq (NonEmptySeq a, NonEmptySeq b), Maybe (NonEmptySeq a, Seq b))
      -> Either a b
      -> (Seq (NonEmptySeq a, NonEmptySeq b), Maybe (NonEmptySeq a, Seq b))
    appendMiddle (sq, mayPair) ei = case ei of
      Left a -> case mayPair of
        Nothing -> (sq, Just (NE.singleton a, empty))
        Just (as, bs) -> case NE.seqToNonEmptySeq bs of
          Nothing -> error "groupEithers: error 1"
          Just neBs -> (sq |> (as, neBs), Just (NE.singleton a, empty))
      Right b -> case mayPair of
        Nothing -> error "groupEithers: error 2"
        Just (as, bs) -> (sq, Just (as, bs |> b))

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
  } deriving (Show, Generic)

instance PrettyVal a => PrettyVal (Slice a) where
  prettyVal = prettySlice Pretty.prettyVal

prettySlice
  :: (a -> Pretty.Value)
  -> Slice a
  -> Pretty.Value
prettySlice f (Slice left sl right) = Pretty.Rec "Slice"
  [ ("_onLeft", prettySeq f left)
  , ("_onSlice", f sl)
  , ("_onRight", prettySeq f right)
  ]

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

-- | Take elements from the head while they match a function.
-- Returns the head elements and the remaining elements.
convertHead
  :: (a -> Maybe b)
  -- ^ Take elements from the head while they return Just.
  -> Seq a
  -> (Seq b, Seq a)
  -- ^ Returns the head elements that matched, and the remaining
  -- elements.
convertHead p = go empty
  where
    go acc sq = case uncons sq of
      Nothing -> (acc, empty)
      Just (x, xs) -> case p x of
        Nothing -> (acc, sq)
        Just a -> go (acc |> a) xs

catMaybes
  :: Seq (Maybe a)
  -> Seq a
catMaybes = foldr f empty
  where
    f e acc = case e of
      Nothing -> acc
      Just x -> x <| acc

-- | If this 'Seq' has exactly one element, returns it.
isSingleton :: Seq a -> Maybe a
isSingleton xs = case viewl xs of
  EmptyL -> Nothing
  y :< ys -> case viewl ys of
    EmptyL -> Just y
    _ -> Nothing
