{-# LANGUAGE RankNTypes #-}
-- | A report showing account totals as a percent of the parent.
module Penny.Pctree where

import qualified Control.Lens as Lens
import Control.Monad (guard, mzero)
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Formatting as F
import qualified Rainbow as RB

import Penny.Account
import Penny.BalanceMap
import Penny.Balgrid
import Penny.Clatch.Types
import Penny.Clatch.Access.Posting
import Penny.Commodity
import Penny.Decimal
import Penny.Polar
import Penny.Report
import Penny.SeqUtil

-- | Make a percent tree report, with typical options already set.
pctree
  :: Commodity
  -- ^ Only this commodity will appear in the report.  All other
  -- commodities are filtered out.
  -> Pole
  -- ^ Only this pole will appear in the report.  Use 'debit' or
  -- 'credit'.  All zero quantities, and quantities on the opposite
  -- side, will be filtered out.
  -> Report
pctree = pctreeOpts (F.prec 3) (flip byStrippedQty)

-- | Make a percent tree report, with all options.
pctreeOpts
  :: (forall r. F.Format r (Double -> r))
  -- ^ How to format the percentages
  -> CmpStrippedBalTree
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
pctreeOpts fmt cmp cy pole _ colors _ clatches
  = case clatchesToStrippedBalMap cy pole clatches of
      Nothing -> emptyMessage
      Just balMap -> printBalgrid colors
        . percentTreeToBalgrid fmt
        . strippedBalTreeToPercentTree
        . sortStrippedBalTree cmp
        . strippedBalMapToStrippedBalTree
        $ balMap
  where
    emptyMessage = Seq.singleton . RB.chunk
      . X.pack $ "(no balances to report)"

clatchesToStrippedBalMap
  :: Commodity
  -> Pole
  -> Seq (Clatch a)
  -> Maybe StrippedBalMap
clatchesToStrippedBalMap cy pole
  = fmap mergeStrippedBalMaps
  . Lens.uncons
  . catMaybes
  . fmap (clatchToStrippedBalMap cy pole)
  where
    mergeStrippedBalMaps (m1, ms) = foldl' appendStrippedBalMap m1 ms

clatchToStrippedBalMap
  :: Commodity
  -- ^ Use this clatch only if it has this commodity
  -> Pole
  -- ^ Use this clatch only if it has this pole
  -> Clatch a
  -> Maybe StrippedBalMap
clatchToStrippedBalMap targetCy targetPole clatch = do
  let cy = Lens.view commodity clatch
  guard $ targetCy == cy
  decPos <- case stripDecimalSign . qty $ clatch of
    Left _ -> mzero
    Right (dp, pole) -> do
      guard $ pole == targetPole
      return dp
  return $ strippedBalMap (Lens.view account clatch) decPos

data PercentTree = PercentTree
  { pctTreeTop :: Double
  , pctTreeLower :: Seq (SubAccount, PercentTree)
  } deriving Show

strippedBalTreeToPercentTree
  :: StrippedBalTree
  -> PercentTree
strippedBalTreeToPercentTree sbTree
  = strippedBalTreeToPercentTreeLoop (strippedBalTreeTop sbTree) sbTree

strippedBalTreeToPercentTreeLoop
  :: StrippedBal
  -- ^ Parent balance
  -> StrippedBalTree
  -> PercentTree
strippedBalTreeToPercentTreeLoop parent strippedBalTree
  = PercentTree (divideDecPositive thisBal parent * 100)
    (Lens.over (Lens.mapped . Lens._2)
               (strippedBalTreeToPercentTreeLoop thisBal) rest)
  where
    thisBal = strippedBalTreeTop strippedBalTree
    rest = strippedBalTreeLower strippedBalTree

percentTreeToBalgrid
  :: (forall r. F.Format r (Double -> r))
  -> PercentTree
  -> Balgrid
percentTreeToBalgrid fmt = go X.empty
  where
    go thisAcct pctTree
      = Balgrid tranche . fmap (uncurry go) . pctTreeLower $ pctTree
      where
        tranche = Tranche (Seq.singleton leftColsLine) thisAcct
          where
            leftColsLine = LeftColsLine Nothing X.empty formatted
              where
                formatted = F.sformat fmt (pctTreeTop pctTree)
