{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Balance where

import Control.Lens
import Penny.Amount
import qualified Data.Foldable as F
import Penny.Decimal
import Penny.Commodity
import Penny.Qty
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Traversable as T

-- | The balance of multiple commodities.
newtype Balance = Balance (M.Map Commodity Qty)
  deriving Show

makeWrapped ''Balance

instance Monoid Balance where
  mempty = Balance M.empty
  mappend (Balance x) (Balance y) = Balance $ M.unionWith (+) x y

c'Balance'Amount :: Amount -> Balance
c'Balance'Amount (Amount c q) = Balance $ M.singleton c q

addAmountToBalance :: Amount -> Balance -> Balance
addAmountToBalance (Amount c q) (Balance m) = Balance . M.alter f c $ m
  where
    f v = case v of
      Nothing -> Just q
      Just l -> Just $ q + l

isBalanced :: Balance -> Bool
isBalanced (Balance m) = F.all isZero m
  where
    isZero (Qty (Exponential signif _)) = signif == 0

newtype Imbalance = Imbalance (M.Map Commodity QtyNonZero)
  deriving Show

c'Imbalance'Amount :: Amount -> Imbalance
c'Imbalance'Amount = c'Imbalance'Balance . c'Balance'Amount

c'Imbalance'Balance :: Balance -> Imbalance
c'Imbalance'Balance (Balance m)
  = Imbalance
  . M.mapMaybe qtyToQtyNonZero $ m

c'Balance'Imbalance :: Imbalance -> Balance
c'Balance'Imbalance (Imbalance m)
  = Balance . fmap toQty $ m

instance Monoid Imbalance where
  mempty = Imbalance M.empty
  mappend x y =
    c'Imbalance'Balance $ mappend
      (c'Balance'Imbalance x) (c'Balance'Imbalance y)


-- | Thing that is accompanied by a running balance.
data RunningBalance a = RunningBalance
  { _runningBalance :: Balance
  , _runningBalancee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''RunningBalance

runningBalances
  :: Traversable t
  => t Amount
  -> t (RunningBalance ())
runningBalances = snd . T.mapAccumL addBal mempty
  where
    addBal acc am = (acc', new)
      where
        acc' = acc <> c'Balance'Amount am
        new = RunningBalance acc' ()
