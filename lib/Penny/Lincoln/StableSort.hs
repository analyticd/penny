-- | Monadic stable sort.
module Penny.Lincoln.StableSort
  ( impureCmpSortByM
  , combineKeys
  , SortKey(..)
  , pureCmpSortByM
  , reverseOrder
  , mapKey
  ) where

import Control.Applicative
import Data.Sequence
import qualified Data.Sequence as S
import Data.Ord
import Prelude hiding (zip)
import qualified Data.Traversable as T
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

pureCmpSortByM
  :: Applicative f
  => SortKey f k a
  -> Seq a
  -> f (Seq a)
pureCmpSortByM (SortKey cmp get) sq = fmap go $ T.traverse get sq
  where
    go keys
      = fmap snd
      . S.sortBy (\x y -> cmp (fst x) (fst y))
      . zip keys
      $ sq

-- | Stable sort of a 'Seq', with effects.
impureCmpSortByM
  :: Monad m
  => (a -> a -> m Ordering)
  -- ^ Obtain the ordering of two elements.  No guarantee is made
  -- about the behavior of 'impureCmpSortByM' if this function returns
  -- different 'Ordering' when repeatedly handed the same two
  -- elements.
  -> Seq a
  -> m (Seq a)
impureCmpSortByM cmp sq = case viewl sq of
  EmptyL -> return S.empty
  x1 :< xs1 -> case viewl xs1 of
    EmptyL -> return $ singleton x1
    _ -> do
      l <- impureCmpSortByM cmp splitL
      r <- impureCmpSortByM cmp splitR
      merge cmp l r
      where
        (splitL, splitR) = split sq

-- | Combine two sort keys.  This is not a semigroup because it is not
-- associative; @a `combineKeys` (b `combineKeys` c)@ has a different
-- order of effects than @(a `combineKeys` b) `combineKeys` c@.
combineKeys
  :: Monad m
  => (a -> a -> m Ordering)
  -> (a -> a -> m Ordering)
  -> a -> a -> m Ordering
combineKeys fx fy a b = do
  resX <- fx a b
  case resX of
    EQ -> fy a b
    x -> return x

split :: Seq a -> (Seq a, Seq a)
split xs = S.splitAt (S.length xs `div` 2) xs

merge
  :: Monad m
  => (a -> a -> m Ordering)
  -> Seq a
  -> Seq a
  -> m (Seq a)
merge cmp inL inR = case viewl inL of
  EmptyL -> return inR
  x :< xs -> case viewl inR of
    EmptyL -> return inL
    y :< ys -> do
      ord <- cmp x y
      case ord of
        GT -> do
          rest <- merge cmp inL ys
          return $ y <| rest
        _ -> do
          rest <- merge cmp xs inR
          return $ x <| rest
