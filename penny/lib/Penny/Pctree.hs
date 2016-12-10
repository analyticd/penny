-- | A report showing account totals as a percent of the parent.
module Penny.Pctree where

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
import Penny.Clatch.Access.Posting
import Penny.Colors
import Penny.Commodity
import Penny.Copper.Copperize
import Penny.Decimal
import Penny.Polar
import Penny.Popularity
import Penny.Report

-- | Make a percent tree report.
pctree
  :: CmpStrippedBalTree
  -- ^ How to sort the percent tree.  You might use 'byStrippedQty' or
  -- 'bySubAccount' or 'flip' 'byStrippedQty'.
  -> Commodity
  -- ^ Only this commodity will appear in the report.  All other
  -- commodities are filtered out.
  -> Pole
  -- ^ Only this pole will appear in the report.  Use 'debit' or
  -- 'credit'.  All zero quantities, and quantities on the opposite
  -- side, will be filtered out.
  -> Report
pctree = undefined
