-- | The multi-commodity Balance report. This is the simpler balance
-- report because it does not allow for commodities to be converted.

module Penny.Cabin.Balance.MultiCommodity (
  Opts(..),
  defaultOpts,
  defaultParseOpts,
  defaultFormat,
  parseReport,
  defaultReport,
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
import qualified Penny.Cabin.Balance.MultiCommodity.Chunker as K
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors.DarkBackground as CD
import qualified Penny.Cabin.Balance.MultiCommodity.Help as H
import qualified Penny.Cabin.Balance.MultiCommodity.Parser as P
import qualified Penny.Cabin.Interface as I

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

defaultOpts :: Opts
defaultOpts = Opts {
  drCrColors = CD.drCrColors
  , baseColors = CD.baseColors
  , balanceFormat = defaultFormat
  , showZeroBalances = CO.ShowZeroBalances True
  , order = compare
  }

defaultParseOpts :: P.ParseOpts
defaultParseOpts = P.ParseOpts {
  P.drCrColors = CD.drCrColors
  , P.baseColors = CD.baseColors
  , P.colorPref = CO.PrefAuto
  , P.showZeroBalances = CO.ShowZeroBalances True
  , P.order = compare
  }

fromParseOpts ::
  (L.Commodity -> L.Qty -> X.Text)
  -> P.ParseOpts
  -> Opts
fromParseOpts fmt (P.ParseOpts dc bc _ szb o) =
  Opts dc bc fmt szb o

defaultFormat :: a -> L.Qty -> X.Text
defaultFormat _ = X.pack . show . L.unQty

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

-- | This report is what to use if you already have your options (that
-- is, you are not parsing them in from the command line.)
report :: Opts -> [L.Box a] -> [Chunk.Chunk]
report (Opts dc bc bf szb o) =
  K.rowsToChunks bf dc bc
  . rows
  . summedSortedBalTree szb o

-- | The MultiCommodity report with configurable options that have
-- been parsed from the command line.
parseReport ::
  (L.Commodity -> L.Qty -> X.Text)
  -- ^ How to format balances. For instance you can use this to
  -- perform commodity-sensitive digit grouping.

  -> P.ParseOpts
  -- ^ Default options for the report. These can be overriden on the
  -- command line.

  -> I.Report
parseReport fmt o = I.Report H.help "balance" r
  where
    r = fmap f P.parseOptions
      where
        f toOs' rt _ _ bs _ = return $ Chunk.chunksToText col chunks
          where
            os' = toOs' o
            mcOpts = fromParseOpts fmt os'
            chunks = report mcOpts bs
            col = CO.autoColors (P.colorPref os') rt


-- | The MultiCommodity report, with default options.
defaultReport :: I.Report
defaultReport = parseReport defaultFormat defaultParseOpts
