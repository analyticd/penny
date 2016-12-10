module Penny.BalanceMap
  ( BalanceMap
  , topLevelBalance
  , lowerBalances
  , balanceMap
  , BalanceTree
  , balTreeTop
  , balTreeLower
  , balanceMapToBalanceTree
  , CmpBalanceTree
  , sortBalanceTree
  , byCommodity
  , bySubAccount
  ) where

import Control.Lens (uncons)
import qualified Control.Lens as Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Penny.Account
import Penny.Balance
import Penny.Commodity
import Penny.Decimal

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

-- | Holds the balances for a hierarchical set of accounts.  Unlike a
-- 'BalanceMap', this can be sorted.
data BalanceTree = BalanceTree
  { balTreeTop :: Balance
  , balTreeLower :: Seq (SubAccount, BalanceTree)
  } deriving Show

-- | Converts a 'BalanceMap' to a 'BalanceTree'.
balanceMapToBalanceTree :: BalanceMap -> BalanceTree
balanceMapToBalanceTree (BalanceMap top lower)
  = BalanceTree top
  . Seq.fromList
  . fmap (Lens.over Lens._2 balanceMapToBalanceTree)
  . M.assocs
  $ lower

type CmpBalanceTree
  = (SubAccount, BalanceTree)
  -> (SubAccount, BalanceTree)
  -> Ordering

-- | Sorts a 'BalanceTree'.  Typically used with 'byCommodity', as in
--
-- > 'sortBalanceTree' ('byCommodity' \"$\") balTree

sortBalanceTree
  :: CmpBalanceTree
  -> BalanceTree
  -> BalanceTree
sortBalanceTree cmp (BalanceTree top lower)
  = BalanceTree top (Seq.sortBy cmp lower)

-- | Returns a sort function that sorts 'Balance's in ascending order
-- by the given commodity.  A 'Balance' that does not have the
-- commodity is less than a 'Balance' that does have the commodity.
--
-- Sorts in ascending order.  To sort in descending order, use 'flip',
-- like
--
-- > 'flip' ('byCommodity' \"\$\")
byCommodity
  :: Commodity
  -- ^ Sort by this commodity
  -> CmpBalanceTree
byCommodity cy (_, BalanceTree (Balance l) _) (_, BalanceTree (Balance r) _)
  = case (M.lookup cy l, M.lookup cy r) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing) -> GT
      (Nothing, Just _) -> LT
      (Just x, Just y) -> cmpDecimal compare x y

-- | Returns a sort function that sorts 'Balance's using the given
-- function to compare the 'SubAccount's.
bySubAccount
  :: (SubAccount -> SubAccount -> Ordering)
  -> CmpBalanceTree
bySubAccount f (l, _) (r, _) = f l r
