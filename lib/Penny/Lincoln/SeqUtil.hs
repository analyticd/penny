-- | Utilities for "Data.Sequence".

module Penny.Lincoln.SeqUtil where

import Control.Applicative hiding (empty)
import Data.Sequence
import qualified Data.Traversable as T
import qualified Data.Sequence as S
import Data.Functor.Contravariant

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
  :: Applicative f
  => SortKey f k a
  -> Seq a
  -> f (Seq a)
sortByM (SortKey cmp get) sq = fmap go $ T.traverse get sq
  where
    go keys
      = fmap snd
      . S.sortBy (\x y -> cmp (fst x) (fst y))
      . S.zip keys
      $ sq

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
