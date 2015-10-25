{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | Pairings of commodities and decimal amounts.
module Penny.Balance where

import Control.Lens
import Penny.Amount
import qualified Data.Foldable as F
import Penny.Decimal
import Penny.Commodity
import qualified Data.Map as M
import Penny.NonZero (integerToNonZero, nonZeroToInteger)

-- | The balance of multiple commodities.
newtype Balance = Balance (M.Map Commodity Decimal)
  deriving Show

makeWrapped ''Balance

instance Monoid Balance where
  mempty = Balance M.empty
  mappend (Balance x) (Balance y) = Balance $ M.unionWith (+) x y

c'Balance'Amount :: Amount -> Balance
c'Balance'Amount (Amount c q) = Balance $ M.singleton c q

-- | True if the 'Balance' contains no commodities or if each
-- commodity has a zero balance.
isBalanced :: Balance -> Bool
isBalanced (Balance m) = F.all isZero m
  where
    isZero (Exponential signif _) = signif == 0

-- | Multiple commodities, where none of the commodities are balanced.
newtype Imbalance = Imbalance (M.Map Commodity DecNonZero)
  deriving Show

c'Imbalance'Amount :: Amount -> Imbalance
c'Imbalance'Amount = c'Imbalance'Balance . c'Balance'Amount

c'Imbalance'Balance :: Balance -> Imbalance
c'Imbalance'Balance (Balance m)
  = Imbalance
  . M.mapMaybe qtyToQtyNonZero $ m
  where
    qtyToQtyNonZero d = case d ^. coefficient . to integerToNonZero of
      Nothing -> Nothing
      Just nz -> Just $ d & coefficient .~ nz

c'Balance'Imbalance :: Imbalance -> Balance
c'Balance'Imbalance (Imbalance m)
  = Balance . fmap (over coefficient nonZeroToInteger) $ m

instance Monoid Imbalance where
  mempty = Imbalance M.empty
  mappend x y =
    c'Imbalance'Balance $ mappend
      (c'Balance'Imbalance x) (c'Balance'Imbalance y)

