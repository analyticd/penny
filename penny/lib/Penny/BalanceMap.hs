module Penny.BalanceMap
  ( BalanceMap
  , topLevelBalance
  , lowerBalances
  , balanceMap
  ) where

import Control.Lens (uncons)
import Data.Map (Map)
import qualified Data.Map as M

import Penny.Account
import Penny.Balance

-- | Holds the balances for a hierarchical set of accounts.  To create
-- a 'BalanceMap' from a single 'Balance', use 'balanceMap'; then use
-- 'mappend' to aggregate several 'BalanceMap' into one.
data BalanceMap = BalanceMap
  { topLevelBalance :: Balance
  , lowerBalances :: Map SubAccount BalanceMap
  } deriving Show

instance Monoid BalanceMap where
  mempty = BalanceMap mempty M.empty

  mappend (BalanceMap b1 m1) (BalanceMap b2 m2)
    = BalanceMap (b1 `mappend` b2) (M.unionWith mappend m1 m2)

balanceMap :: Account -> Balance -> BalanceMap
balanceMap sq bal = BalanceMap bal $ case uncons sq of
  Nothing -> M.empty
  Just (a1, as) -> M.singleton a1 (balanceMap as bal)
