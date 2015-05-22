{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Sorted where

import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import Penny.Serial
import Data.Foldable

-- | A single sort key.
data SortKey f k a = SortKey
  { _orderer :: (k -> k -> Ordering)
  , _puller :: (a -> f k)
  }

makeLenses ''SortKey

instance Contravariant (SortKey f k) where
  contramap f (SortKey cmp get) = SortKey cmp (get . f)

newtype Sorter m a = Sorter { sort :: (Seq a -> m (Seq a)) }

makeWrapped ''Sorter

instance Monad m => Monoid (Sorter m a) where
  mempty = Sorter return
  mappend (Sorter f) (Sorter g) = Sorter (f >=> g)

mapKey
  :: Monad f
  => (k -> k')
  -> (k' -> k)
  -> SortKey f k a
  -> SortKey f k' a
mapKey fwd bak (SortKey cmp get) = SortKey cmp' get'
  where
    get' = fmap (liftM fwd) get
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
  -> Sorter m a
sortByM (SortKey cmp get) = Sorter $ \sq -> liftM (go sq) $ T.mapM get sq
  where
    go sq keys
      = fmap snd
      . S.sortBy (\x y -> cmp (fst x) (fst y))
      . S.zip keys
      $ sq

newtype Sorted a = Sorted (Sersetted a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Sorted

sortWithSersetted
  :: Monad m
  => Sorter m a
  -> Seq a
  -> m (Seq (Sorted a))
sortWithSersetted srtr = liftM (fmap Sorted) . liftM assignSersetted
  . sort srtr
