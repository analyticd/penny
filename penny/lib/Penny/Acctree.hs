-- | The account tree report.
module Penny.Acctree where

import qualified Control.Lens as Lens
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Text as X

import Penny.Account
import Penny.Balance
import Penny.BalanceMap
import Penny.Balgrid
import Penny.Clatcher
import Penny.Clatch
import Penny.Clatch.Access.Balance
import Penny.Clatch.Access.Posting
import Penny.Colors
import Penny.Commodity
import Penny.Copper.Copperize
import Penny.Decimal
import Penny.Popularity

-- | Make an account tree report.
acctree
  :: CmpBalanceTree
  -- ^ How to sort the balance tree.  You might use 'byCommodity' or
  -- 'bySubAccount'.
  -> Report
acctree cmp _ colors hist clatches
  = printBalgrid colors
  . balanceTreeToBalgrid colors hist (X.empty)
  . sortBalanceTree cmp
  . balanceMapToBalanceTree
  $ balMap
  where
    balMap = foldl' mappend mempty . fmap mkBalMap $ clatches
    mkBalMap clatch = balanceMap (Lens.view account clatch)
      (Lens.view balance clatch)


balanceTreeToBalgrid
  :: Colors
  -> History
  -> SubAccount
  -> BalanceTree
  -> Balgrid
balanceTreeToBalgrid colors hist sub balTree
  = Balgrid (subAccountTranche hist sub (balTreeTop balTree))
  . fmap mkLowerTree
  . balTreeLower
  $ balTree
  where
    mkLowerTree (sub', balTree)
      = balanceTreeToBalgrid colors hist sub' balTree

subAccountTranche
  :: History
  -> SubAccount
  -> Balance
  -> Tranche
subAccountTranche hist sub bal
  = Tranche (balanceToLeftCols hist bal) sub

balanceToLeftCols
  :: History
  -> Balance
  -> Seq LeftColsLine
balanceToLeftCols hist (Balance mp)
  = fmap (balanceLineToLeftCol hist)
  . Seq.fromList
  . Map.assocs
  $ mp

balanceLineToLeftCol
  :: History
  -> (Commodity, Decimal)
  -> LeftColsLine
balanceLineToLeftCol hist (cy, dec) = LeftColsLine mayPole cy qtyTxt
  where
    qtyTxt = decimalText grouper dec
    grouper = either (Left . Just) (Right . Just)
      . bestGrouperForCommodity hist
      $ cy
    mayPole = case stripDecimalSign dec of
      Left _ -> Nothing
      Right (_, pole) -> Just pole
