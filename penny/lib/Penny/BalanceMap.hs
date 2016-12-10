module Penny.BalanceMap
  ( -- * BalanceMap
    BalanceMap
  , topLevelBalance
  , lowerBalances
  , balanceMap
  , BalanceTree
  , balTreeTop
  , balTreeLower
  , balanceMapToBalanceTree
  , CmpBalanceTree
  , sortBalanceTree
  , byQty

  -- * Sorting - common to 'BalanceTree' and 'StrippedBalTree'
  , bySubAccountCmp
  , bySubAccount

  -- * StrippedBalMap
  , StrippedBal
  , StrippedBalMap
  , strippedBalMapTop
  , strippedBalMapLower
  , strippedBalMap
  , appendStrippedBalMap
  , StrippedBalTree
  , strippedBalTreeTop
  , strippedBalTreeLower
  , strippedBalMapToStrippedBalTree
  , CmpStrippedBalTree
  , sortStrippedBalTree
  , byStrippedQty
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

-- | Sorts a 'BalanceTree'.  Typically used with 'byQty', as in
--
-- > sortBalanceTree (byQty "$") balTree

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
-- > flip (byQty "$")
byQty
  :: Commodity
  -- ^ Sort by this commodity
  -> CmpBalanceTree
byQty cy (_, BalanceTree (Balance l) _) (_, BalanceTree (Balance r) _)
  = case (M.lookup cy l, M.lookup cy r) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing) -> GT
      (Nothing, Just _) -> LT
      (Just x, Just y) -> cmpDecimal compare x y

-- | Returns a sort function that sorts 'Balance's using the given
-- function to compare the 'SubAccount's.
--
-- @
-- 'bySubAccountCmp'
--   :: ('SubAccount' -> 'SubAccount' -> 'Ordering')
--   -> 'CmpBalanceTree'
-- 'bySubAccountCmp'
--   :: ('SubAccount' -> 'SubAccount' -> 'Ordering')
--   -> 'CmpStrippedBalTree'
-- @
bySubAccountCmp
  :: (SubAccount -> SubAccount -> Ordering)
  -> (SubAccount, a)
  -> (SubAccount, b)
  -> Ordering
bySubAccountCmp f l r = f (fst l) (fst r)

-- | Returns a sort function that sorts by 'SubAccount' in ascending order.
--
-- @
-- 'bySubAccount' :: 'CmpBalanceTree'
-- 'bySubAccount' :: 'CmpStrippedBalTree'
-- @
bySubAccount :: (SubAccount, a) -> (SubAccount, b) -> Ordering
bySubAccount = bySubAccountCmp compare

-- | A 'StrippedBalance' is a balance that can only have a single
-- polarity (it cannot be a debit or credit; it can be only one or the
-- other) and that only represents a single commodity.
type StrippedBal = DecPositive

-- | Holds 'StrippedBal' for a hierarchical set of accounts.  To
-- create a 'StrippedBalMap' from a single 'StrippedBal', use
-- 'strippedBalMap', then use 'mappend' to aggregate several
-- 'StrippedBalMap' into one.
data StrippedBalMap = StrippedBalMap
  { strippedBalMapTop :: StrippedBal
  , strippedBalMapLower :: Map SubAccount StrippedBalMap
  } deriving Show

appendStrippedBalMap :: StrippedBalMap -> StrippedBalMap -> StrippedBalMap
appendStrippedBalMap (StrippedBalMap b1 m1) (StrippedBalMap b2 m2)
    = StrippedBalMap (addDecPositive b1 b2)
                     (M.unionWith appendStrippedBalMap m1 m2)

strippedBalMap :: Account -> StrippedBal -> StrippedBalMap
strippedBalMap sq bal = StrippedBalMap bal $ case uncons sq of
  Nothing -> M.empty
  Just (a1, as) -> M.singleton a1 (strippedBalMap as bal)

-- | Holds the balances for a hierarchical set of accounts.  Unlike a
-- 'StrippedBalMap', this can be sorted.
data StrippedBalTree = StrippedBalTree
  { strippedBalTreeTop :: StrippedBal
  , strippedBalTreeLower :: Seq (SubAccount, StrippedBalTree)
  } deriving Show

-- | Converts a 'StrippedBalMap' to a 'StrippedBalTree'.
strippedBalMapToStrippedBalTree :: StrippedBalMap -> StrippedBalTree
strippedBalMapToStrippedBalTree (StrippedBalMap top lower)
  = StrippedBalTree top
  . Seq.fromList
  . fmap (Lens.over Lens._2 strippedBalMapToStrippedBalTree)
  . M.assocs
  $ lower

type CmpStrippedBalTree
  = (SubAccount, StrippedBalTree)
  -> (SubAccount, StrippedBalTree)
  -> Ordering

-- | Sorts a 'StrippedBalTree'.  Typically used with 'byStrippedQty',
-- as in
--
-- > sortStrippedBalTree byStrippedQty x

sortStrippedBalTree
  :: CmpStrippedBalTree
  -> StrippedBalTree
  -> StrippedBalTree
sortStrippedBalTree cmp (StrippedBalTree top lower)
  = StrippedBalTree top (Seq.sortBy cmp lower)

-- | Returns a sort function that sorts 'StrippedBal's in ascending
-- order.  To sort in descending order, use 'flip', like
--
-- > flip byStrippedQty

byStrippedQty :: CmpStrippedBalTree
byStrippedQty (_, StrippedBalTree l _) (_, StrippedBalTree r _)
  = cmpPositive compare l r

