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
  , concatGroups
  , leaders
  , middle
  , trailers
  , groupEithers
  , filterM
  , intersperse
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

  -- * Sorting 'NonEmptySeq'
  , sortNonEmptySeq
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
  } deriving (Eq, Show, Generic)

makeLenses ''Groups

concatGroups :: Groups a b -> Seq (Either a b)
concatGroups (Groups l m r)
  = fmap Right l
  <> mids
  <> fmap Left r
  where
    convertPair (a, b)
      = fmap Left (NE.nonEmptySeqToSeq a)
      <> fmap Right (NE.nonEmptySeqToSeq b)
    mids = join . fmap convertPair $ m

-- | Pull all @a@ off the front of the sequence.  Returns all the @a@,
-- and what remains, which is either nothing at all, or a @b@ and the
-- rest of the sequence.
frontAs
  :: Seq (Either a b)
  -> (Seq a, Maybe (b, Seq (Either a b)))
frontAs = go empty
  where
    go acc sq = case viewl sq of
      EmptyL -> (acc, Nothing)
      a :< as -> case a of
        Left a -> go (acc |> a) as
        Right b -> (acc, Just (b, as))


-- | Pull all @b@ off the front of the sequence.  Returns all the @b@,
-- and what remains, which is either nothing at all, or an @a@ and the
-- rest of the sequence.
frontBs
  :: Seq (Either a b)
  -> (Seq b, Maybe (a, Seq (Either a b)))
frontBs = go empty
  where
    go acc sq = case viewl sq of
      EmptyL -> (acc, Nothing)
      a :< as -> case a of
        Right b -> go (acc |> b) as
        Left a -> (acc, Just (a, as))


group
  :: (a, Seq (Either a b))
  -> (NonEmptySeq a, Maybe (NonEmptySeq b, Maybe (a, Seq (Either a b))))
  -- ^ Always returns the @a@ at the front.  Then, two possibilities.
  -- One is that there is at least one @b@, in which case we have a
  -- group.  If there is a group, then there might be remaining
  -- eithers.  If so, they have a leading @a@.
group (a, eithers) = (NE.NonEmptySeq a as0, mayResultPair)
  where
    (as0, mayBAndRest) = frontAs eithers
    mayResultPair = fmap f mayBAndRest
      where
        f (b, eithers) = (NE.NonEmptySeq b bs0, mayResultPairB)
          where
            (bs0, mayResultPairB) = frontBs eithers

groups
  :: (a, Seq (Either a b))
  -> (Seq (NonEmptySeq a, NonEmptySeq b), Seq a)
groups = go empty
  where
    go acc a = case maybeGroup of
      Nothing -> (acc, NE.nonEmptySeqToSeq as)
      Just (bs, maybeEithers) -> case maybeEithers of
        Nothing -> (allPairs, empty)
        Just eithers -> go allPairs eithers
        where
          allPairs = acc |> (as, bs)
      where
        (as, maybeGroup) = group a


groupEithers
  :: Seq (Either a b)
  -> Groups a b
groupEithers eithers =
  let (bs, mayAAndRest) = frontBs eithers
  in case mayAAndRest of
      Nothing -> Groups bs empty empty
      Just aAndRest -> let (gs, as) = groups aAndRest
        in Groups bs gs as


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

-- | Stable sort on a 'NonEmptySeq'.
sortNonEmptySeq
  :: (a -> a -> Ordering)
  -> NonEmptySeq a
  -> NonEmptySeq a
sortNonEmptySeq by
  = maybe (error "sortNonEmptySeq: seq is empty") id
  . NE.seqToNonEmptySeq
  . sortBy by
  . NE.nonEmptySeqToSeq
