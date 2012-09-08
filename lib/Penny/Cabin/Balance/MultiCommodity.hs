-- | The multi-commodity Balance report. This is the simpler balance
-- report because it does not allow for commodities to be converted.

module Penny.Cabin.Balance.MultiCommodity (
  Opts(..),
  report
  ) where

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Lincoln as L
import qualified Data.Map as M
import qualified Penny.Cabin.Options as CO
import Data.Monoid (mappend, mempty)
import qualified Data.Text as X
import qualified Data.Tree as E
import qualified Penny.Cabin.Balance.Chunker as K
import qualified Penny.Cabin.Chunk as Chunk

-- | Options for making the balance report. These are the only options
-- needed to make the report if the options are not being parsed in
-- from the command line.
data Opts = Opts {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccountName -> L.SubAccountName -> Ordering
  }

summedSortedBalTree ::
  CO.ShowZeroBalances
  -> (L.SubAccountName -> L.SubAccountName -> Ordering)
  -> [L.Box a]
  -> (E.Forest (L.SubAccountName, L.Balance), L.Balance)
summedSortedBalTree szb o =
  U.sumForest mempty mappend
  . U.sortForest o'
  . U.balances szb
  where
    o' x y = o (fst x) (fst y)

rows ::
  (E.Forest (L.SubAccountName, L.Balance), L.Balance)
  -> [K.Row]
rows (o, b) = first:rest
  where
    first = K.Row 0 (X.pack "Total") (M.assocs . L.unBalance $ b)
    rest = map row . concatMap E.flatten . map U.labelLevels $ o
    row (l, (s, ib)) =
      K.Row l (L.text s) (M.assocs . L.unBalance $ ib)

report :: Opts -> [L.Box a] -> [Chunk.Chunk]
report (Opts dc bc bf szb o) =
  K.rowsToChunks bf dc bc
  . rows
  . summedSortedBalTree szb o
