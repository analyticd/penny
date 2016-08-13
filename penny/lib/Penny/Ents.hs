{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | A valid transaction must be balanced--that is, for each
-- commodity, the debits must be equal to the credits.  This module
-- contains types and functions that help ensure that an entire
-- collection of postings--that is, a transaction--is balanced.
module Penny.Ents
  ( Ents
  , entsToSeqEnt
  , entsToImbalance
  , appendEnt
  , prependEnt
  , appendTrio
  , prependTrio
  , Balanced
  , balancedToSeqEnt
  , entsToBalanced
  , ImbalancedError(..)
  , restrictedBalanced
  ) where

import Control.Lens ((<|), (|>))
import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)))
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import qualified Data.Sequence as S
import Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Commodity
import Penny.Copper.Decopperize (dBrimAnyRadix)
import Penny.Decimal
import Penny.Polar
import Penny.Rep
import Penny.Trio
import qualified Penny.Troika as Y

-- | An 'Ents' contains a collection of postings.  The collection is
-- not necessarily balanced.  The 'Ents' keeps track of whether the
-- collection of postings is imbalanced and, if so, by how much.
--
-- Each posting is represented only by a 'Y.Troika' along with any
-- arbitrary metadata.  Typically the metadata will have a 'Seq' of
-- 'Penny.Tree.Tree' to represent arbitrary metadata, and a 'Trio'
-- to hold the original representation of the quantity and
-- commodity.
data Ents a = Ents
  { entsToSeqEnt :: Seq (Y.Troika, a)
  , entsToImbalance :: Imbalance
  } deriving Show

instance Functor Ents where
  fmap f (Ents s b) = Ents (fmap (fmap f) s) b

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance Foldable Ents where
  foldr f z = F.foldr f z . fmap snd . entsToSeqEnt

instance Traversable Ents where
  sequenceA (Ents sq bl) = fmap (flip Ents bl) . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

-- | Contains a sequence of 'Y.Troika' along with arbitrary metadata
-- but, unlike an 'Ents', this is always guaranteed to be balanced.
newtype Balanced a = Balanced { balancedToSeqEnt :: Seq (Y.Troika, a) }
  deriving Show

instance Functor Balanced where
  fmap f (Balanced sq) = Balanced $ fmap (fmap f) sq

instance Monoid (Balanced a) where
  mempty = Balanced mempty
  mappend (Balanced x) (Balanced y) = Balanced (x <> y)

instance Foldable Balanced where
  foldr f z = F.foldr f z . fmap snd . balancedToSeqEnt

instance Traversable Balanced where
  sequenceA (Balanced sq) = fmap Balanced . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

appendEnt :: Ents a -> (Amount, a) -> Ents a
appendEnt (Ents s b) (am@(Amount cy q), e) = Ents (s |> (tm, e))
  (b <> c'Imbalance'Amount am)
  where
    tm = Y.Troika cy (Right q)

prependEnt :: (Amount, a) -> Ents a -> Ents a
prependEnt (am@(Amount cy q), e) (Ents s b) = Ents ((tm, e) <| s)
  (b <> c'Imbalance'Amount am)
  where
    tm = Y.Troika cy (Right q)

appendTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
appendTrio (Ents sq imb) trio = fmap f $ Y.trioToTroiload imb trio
  where
    f (troiload, cy) = \meta -> Ents (sq |> (tm, meta)) imb'
      where
        tm = Y.Troika cy (Left troiload)
        imb' = imb <> c'Imbalance'Amount (Y.c'Amount'Troika tm)

prependTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
prependTrio (Ents sq imb) trio = fmap f $ Y.trioToTroiload imb trio
  where
    f (troiload, cy) = \meta -> Ents ((tm, meta) <| sq) imb'
      where
        tm = Y.Troika cy (Left troiload)
        imb' = imb <> c'Imbalance'Amount (Y.c'Amount'Troika tm)


data ImbalancedError
  = ImbalancedError (Commodity, DecNonZero) [(Commodity, DecNonZero)]
  deriving Show

entsToBalanced :: Ents a -> Either ImbalancedError (Balanced a)
entsToBalanced (Ents sq (Imbalance m)) = case M.toList m of
  [] -> return $ Balanced sq
  x:xs -> Left $ ImbalancedError x xs

-- | Creates a 'Balanced'; never fails.  Less flexible than
-- 'entsToBalanced' but guaranteed not to fail.
restrictedBalanced
  :: NonEmptySeq (BrimAnyRadix, a)
  -- ^ List of quantities and metadata.  There must be at least one quantity.
  -> Pole
  -- ^ All given quantities will be on this side.
  -> Commodity
  -- ^ All quantities will have this commodity.
  -> Arrangement
  -- ^ All given quantities and commodities will be arranged in this
  -- fashion.
  -> a
  -- ^ Metadata for the offsetting 'Y.Troika'
  -> Balanced a
  -- ^ A single offsetting 'Y.Troika' is created that is a 'Y.E' that
  -- is on the opposite side of the quantities given above.  All other
  -- 'Y.Troika' are 'Y.QC'.
restrictedBalanced ne pole cy ar meta
  = Balanced ((fmap mkPair $ NE.nonEmptySeqToSeq ne) |> offset)
  where
    mkPair (brim, meta) = (Y.Troika cy . Left $ Y.QC rar ar, meta)
      where
        rar = c'RepAnyRadix'BrimAnyRadix pole brim
    offset = (Y.Troika cy . Left $ Y.E dnz, meta)
      where
        dnz = c'DecNonZero'DecPositive (opposite pole) $
          foldl addDecPositive b1 bs
          where
            NE.NonEmptySeq b1 bs = fmap (dBrimAnyRadix . fst) ne

-- | Creates a 'Balanced'; never fails.  Unlike 'restrictedBalanced'
-- this function creates only transactions that have exactly two
-- postings.  The advantage of this function over 'restrictedBalanced'
-- is that the quantity can be zero.
twoPostingBalanced
  :: (RepAnyRadix, a)
  -- ^ Quantity and metadata for main posting
  -> Commodity
  -- ^ Both postings will have this commodity
  -> Arrangement
  -- ^ Both postings will be arranged in this fashion.
  -> a
  -- ^ Metadata for the offsetting posting
  -> Balanced a
twoPostingBalanced = undefined
