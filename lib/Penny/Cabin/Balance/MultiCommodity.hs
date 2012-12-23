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

import Control.Applicative (Applicative, pure)
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Data.Either as Ei
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
import qualified System.Console.MultiArg as MA
import qualified Penny.Shield as S

-- | Options for making the balance report. These are the only options
-- needed to make the report if the options are not being parsed in
-- from the command line.
data Opts = Opts
  { drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  }

defaultOpts :: Opts
defaultOpts = Opts
  { drCrColors = CD.drCrColors
  , baseColors = CD.baseColors
  , balanceFormat = defaultFormat
  , showZeroBalances = CO.ShowZeroBalances True
  , order = compare
  }

defaultParseOpts :: P.ParseOpts
defaultParseOpts = P.ParseOpts
  { P.drCrColors = CD.drCrColors
  , P.baseColors = CD.baseColors
  , P.colorPref = const Chunk.Colors0
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
defaultFormat _ = X.pack . show

summedSortedBalTree ::
  CO.ShowZeroBalances
  -> (L.SubAccount -> L.SubAccount -> Ordering)
  -> [L.Box a]
  -> (E.Forest (L.SubAccount, L.Balance), L.Balance)
summedSortedBalTree szb o =
  U.sumForest mempty mappend
  . U.sortForest o'
  . U.balances szb
  where
    o' x y = o (fst x) (fst y)

rows ::
  (E.Forest (L.SubAccount, L.Balance), L.Balance)
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
--parseReport fmt o = (H.help, makeMode)I.Report H.help "balance" r
parseReport fmt o = (H.help, makeMode)
  where
    makeMode rt _ _ fsf = MA.Mode
      { MA.mId = ()
      , MA.mName = "balance"
      , MA.mIntersperse = MA.Intersperse
      , MA.mOpts = map (fmap Right) P.allSpecs
      , MA.mPosArgs = Left
      , MA.mProcess = process fmt o rt fsf
      }

process
  :: Applicative f
  => (L.Commodity -> L.Qty -> X.Text)
  -> P.ParseOpts
  -> S.Runtime
  -> ([L.Transaction] -> [L.Box Ly.LibertyMeta])
  -> [Either String (P.ParseOpts -> P.ParseOpts)]
  -> f ([String], I.PrintReport)
process fmt o rt fsf ls =
  let (posArgs, fns) = Ei.partitionEithers ls
      mkParsedOpts = foldl (flip (.)) id fns
      os' = mkParsedOpts o
      mcOpts = fromParseOpts fmt os'
      pr txns _ =
        let col = P.colorPref os' rt
            chunks = report mcOpts (fsf txns)
            txt = Chunk.chunksToText col chunks
        in return txt
  in pure (posArgs, pr)


-- | The MultiCommodity report, with default options.
defaultReport :: I.Report
defaultReport = parseReport defaultFormat defaultParseOpts
