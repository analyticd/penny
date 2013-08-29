-- | The multi-commodity Balance report. This is the simpler balance
-- report because it does not allow for commodities to be converted.

module Penny.Cabin.Balance.MultiCommodity (
  Opts(..),
  defaultOpts,
  defaultParseOpts,
  parseReport,
  defaultReport,
  report
  ) where

import Control.Applicative (Applicative, pure)
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Scheme.Schemes as Schemes
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Data.Either as Ei
import qualified Data.Map as M
import qualified Penny.Cabin.Options as CO
import Data.Monoid ((<>))
import qualified Data.Text as X
import qualified Data.Tree as E
import qualified Penny.Cabin.Balance.MultiCommodity.Chunker as K
import qualified Penny.Cabin.Balance.MultiCommodity.Parser as P
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Parsers as CP
import qualified System.Console.MultiArg as MA
import qualified System.Console.Rainbow as R

-- | Options for making the balance report. These are the only options
-- needed to make the report if the options are not being parsed in
-- from the command line.
data Opts = Opts
  { balanceFormat :: L.Amount L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  , textFormats :: E.Changers
  }

defaultOpts :: (L.Amount L.Qty -> X.Text) -> Opts
defaultOpts fmt = Opts
  { balanceFormat = fmt
  , showZeroBalances = CO.ShowZeroBalances True
  , order = compare
  , textFormats = Schemes.darkLabels
  }

defaultParseOpts :: P.ParseOpts
defaultParseOpts = P.ParseOpts
  { P.showZeroBalances = CO.ShowZeroBalances False
  , P.order = CP.Ascending
  }

fromParseOpts
  :: E.Changers
  -> (L.Amount L.Qty -> X.Text)
  -> P.ParseOpts
  -> Opts
fromParseOpts chgrs fmt (P.ParseOpts szb o) = Opts fmt szb o' chgrs
  where
    o' = case o of
       CP.Ascending -> compare
       CP.Descending -> CO.descending compare

summedSortedBalTree ::
  CO.ShowZeroBalances
  -> (L.SubAccount -> L.SubAccount -> Ordering)
  -> [(a, L.Posting)]
  -> (E.Forest (L.SubAccount, L.Balance), L.Balance)
summedSortedBalTree szb o ps = (forest, bal)
  where
    (topBal, unsorted) = U.balances szb ps
    (forest, forestSum) = U.sumForest
                        . U.sortForest o'
                        $ unsorted
    bal = topBal <> forestSum
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
report :: Opts -> [(a, L.Posting)] -> [R.Chunk]
report (Opts bf szb o chgrs) =
  K.rowsToChunks chgrs bf
  . rows
  . summedSortedBalTree szb o

-- | The MultiCommodity report with configurable options that have
-- been parsed from the command line.
parseReport
  :: P.ParseOpts
  -- ^ Default options for the report. These can be overriden on the
  -- command line.

  -> I.Report
parseReport o rt = (help o, makeMode)
  where
    makeMode _ _ chgrs _ fsf = MA.modeHelp
      "balance"
      (const (help o))
      (process chgrs o rt fsf)
      (map (fmap Right) P.allSpecs)
      MA.Intersperse
      (return . Left)

process
  :: Applicative f
  => E.Changers
  -> P.ParseOpts
  -> a
  -> ([L.Transaction] -> [(Ly.LibertyMeta, L.Posting)])
  -> [Either String (P.ParseOpts -> P.ParseOpts)]
  -> f I.ArgsAndReport
process chgrs o _ fsf ls =
  let (posArgs, fns) = Ei.partitionEithers ls
      mkParsedOpts = foldl (flip (.)) id fns
      os' = mkParsedOpts o
      pr fmt txns _ = return $ report mcOpts (fsf txns)
        where
          mcOpts = fromParseOpts chgrs fmt os'
  in pure (posArgs, pr)


-- | The MultiCommodity report, with default options.
defaultReport :: I.Report
defaultReport = parseReport defaultParseOpts

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
  , "--order ascending|descending"
  , "  Sort in this order by account name (default: "
    ++ if P.order o == CP.Ascending
       then "ascending" else "descending"
    ++ ")"

  , ""
  , "--help, -h"
  , "  Show this help and exit"
  ]

