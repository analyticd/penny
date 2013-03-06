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
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Data.Either as Ei
import qualified Data.Map as M
import qualified Penny.Cabin.Options as CO
import Data.Monoid (mappend, mempty)
import qualified Data.Text as X
import qualified Data.Tree as E
import qualified Penny.Cabin.Balance.MultiCommodity.Chunker as K
import qualified Penny.Cabin.Balance.MultiCommodity.Parser as P
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Parsers as CP
import qualified System.Console.MultiArg as MA

-- | Options for making the balance report. These are the only options
-- needed to make the report if the options are not being parsed in
-- from the command line.
data Opts = Opts
  { balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  }

defaultOpts :: Opts
defaultOpts = Opts
  { balanceFormat = defaultFormat
  , showZeroBalances = CO.ShowZeroBalances True
  , order = compare
  }

defaultParseOpts :: P.ParseOpts
defaultParseOpts = P.ParseOpts
  { P.showZeroBalances = CO.ShowZeroBalances False
  , P.order = CP.Ascending
  }

fromParseOpts ::
  (L.Commodity -> L.Qty -> X.Text)
  -> P.ParseOpts
  -> Opts
fromParseOpts fmt (P.ParseOpts szb o) = Opts fmt szb o'
  where
    o' = case o of
       CP.Ascending -> compare
       CP.Descending -> CO.descending compare

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
report :: Opts -> [L.Box a] -> [E.PreChunk]
report (Opts bf szb o) =
  K.rowsToChunks bf
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
parseReport fmt o rt = (help o, makeMode)
  where
    makeMode _ _ _ fsf = MA.Mode
      { MA.mName = "balance"
      , MA.mIntersperse = MA.Intersperse
      , MA.mOpts = map (fmap Right) P.allSpecs
      , MA.mPosArgs = Left
      , MA.mProcess = process fmt o rt fsf
      , MA.mHelp = const (help o)
      }

process
  :: Applicative f
  => (L.Commodity -> L.Qty -> X.Text)
  -> P.ParseOpts
  -> a
  -> ([L.Transaction] -> [L.Box Ly.LibertyMeta])
  -> [Either String (P.ParseOpts -> P.ParseOpts)]
  -> f I.ArgsAndReport
process fmt o _ fsf ls =
  let (posArgs, fns) = Ei.partitionEithers ls
      mkParsedOpts = foldl (flip (.)) id fns
      os' = mkParsedOpts o
      mcOpts = fromParseOpts fmt os'
      pr txns _ = return . map Right $ report mcOpts (fsf txns)
  in pure (posArgs, pr)


-- | The MultiCommodity report, with default options.
defaultReport :: I.Report
defaultReport = parseReport defaultFormat defaultParseOpts

------------------------------------------------------------
-- ## Help
------------------------------------------------------------
ifDefault :: Bool -> String
ifDefault b = if b then " (default)" else ""

help :: P.ParseOpts -> String
help o = unlines
  [ "balance"
  , "  Show account balances. Accepts ONLY the following options:"
  , ""
  , "--show-zero-balances"
  , "  Show balances that are zero"
    ++ ifDefault (CO.unShowZeroBalances . P.showZeroBalances $ o)
  , "--hide-zero-balances"
  , "  Hide balances that are zero"
    ++ ifDefault ( not . CO.unShowZeroBalances
                 . P.showZeroBalances $ o)
  , ""
  , "--ascending"
  , "  Sort in ascending order by account name"
    ++ ifDefault (P.order o == CP.Ascending)

  , "--descending"
  , "  Sort in descending order by account name"
    ++ ifDefault (P.order o == CP.Descending)

  , ""
  , "--help, -h"
  , "  Show this help and exit"
  ]

