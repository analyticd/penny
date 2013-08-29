{-# LANGUAGE OverloadedStrings #-}
-- | The Convert report. This report converts all account balances to
-- a single commodity, which must be specified.

module Penny.Cabin.Balance.Convert (
  Opts(..)
  , Sorter
  , report
  , cmdLineReport
  , getSorter
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Tree as E
import qualified Data.Traversable as Tvbl
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as CP
import qualified Penny.Cabin.Scheme as Scheme
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Cabin.Balance.Convert.Chunker as K
import qualified Penny.Cabin.Balance.Convert.ChunkerPct as KP
import qualified Penny.Cabin.Balance.Convert.Options as O
import qualified Penny.Cabin.Balance.Convert.Parser as P
import qualified Penny.Cabin.Interface as I
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Liberty as Ly
import qualified Penny.Shield as S
import qualified Data.Either as Ei
import qualified Data.Map as M
import qualified Data.Text as X
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat, (<>))
import qualified System.Console.MultiArg as MA
import qualified System.Console.Rainbow as Rb

-- | Options for the Convert report. These are the only options you
-- need to use if you are supplying options programatically (as
-- opposed to parsing them in from the command line.)
data Opts = Opts
  { format :: Either (L.Amount L.Qty -> X.Text) P.RoundTo
  -- ^ If you want a convert report that shows a single commodity,
  -- pass a Left showing how to display each amount.  If you want a
  -- convert report that shows percentages, pass a Right here with how
  -- many places to round to.
  , showZeroBalances :: CO.ShowZeroBalances
  , sorter :: Sorter
  , target :: L.To
  , dateTime :: L.DateTime
  , textFormats :: Scheme.Changers
  }

-- | How to sort each line of the report. Each subaccount has only one
-- BottomLine (unlike in the MultiCommodity report, where each
-- subaccount may have more than one BottomLine, one for each
-- commodity.)
type Sorter
  = (L.SubAccount, L.BottomLine)
  -> (L.SubAccount, L.BottomLine)
  -> Ordering

-- | Converts all commodities in a Balance to a single commodity and
-- combines all the BottomLines into one. Fails with an error message
-- if no conversion data is available.
convertBalance
  :: L.PriceDb
  -> L.DateTime
  -> L.To
  -> L.Balance
  -> Ex.Exceptional X.Text L.BottomLine
convertBalance db dt to bal = fmap mconcat r
  where
    r = mapM (convertOne db dt to) . M.assocs . L.unBalance $ bal

-- | Converts a single BottomLine to a new commodity. Fails with an
-- error message if no conversion data is available.
convertOne
  :: L.PriceDb
  -> L.DateTime
  -> L.To
  -> (L.Commodity, L.BottomLine)
  -> Ex.Exceptional X.Text L.BottomLine
convertOne db dt to (cty, bl) =
  case bl of
    L.Zero -> return L.Zero
    L.NonZero (L.Column dc qt) -> Ex.mapExceptional e g ex
      where
        ex = L.convertAsOf db dt to am
        am = L.Amount qt cty
        e = convertError to (L.From cty)
        g r = L.NonZero (L.Column dc r)

-- | Creates an error message for conversion errors.
convertError
  :: L.To
  -> L.From
  -> L.PriceDbError
  -> X.Text
convertError (L.To to) (L.From fr) e =
  let fromErr = L.unCommodity fr
      toErr = L.unCommodity to
  in case e of
    L.FromNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
    L.ToNotFound ->
      X.pack "no data to convert to commodity "
      `X.append` toErr
    L.CpuNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
      `X.append` (X.pack " to commodity ")
      `X.append` toErr
      `X.append` (X.pack " at given date and time")


-- | Create a price database.
buildDb :: [L.PricePoint] -> L.PriceDb
buildDb = foldl f L.emptyDb where
  f db pb = L.addPrice db pb

-- | All data for the report after all balances have been converted to
-- a single commodity and all the sums of the child accounts have been
-- added to the parent accounts.
data ForestAndBL = ForestAndBL {
  _tbForest :: E.Forest (L.SubAccount, L.BottomLine)
  , _tbTotal :: L.BottomLine
  , _tbTo :: L.To
  }

forestToPercents
  :: E.Forest (L.SubAccount, L.BottomLine)
  -> E.Forest (L.SubAccount, Maybe KP.Percent)
forestToPercents ls =
  let tot = sumBottomLines . map (snd . E.rootLabel) $ ls
  in map (treeToPercent tot) ls

treeToPercent
  :: Maybe L.Qty
  -- ^ Sum of all BottomLines at this level
  -> E.Tree (L.SubAccount, L.BottomLine)
  -> E.Tree (L.SubAccount, Maybe KP.Percent)
treeToPercent qty (E.Node (acct, bl) cs) = E.Node (acct, mayPct) cs'
  where
    mayPct = maybe Nothing (flip bottomLineToPercent bl) qty
    cs' = forestToPercents cs

bottomLineToQty :: L.BottomLine -> Maybe (L.DrCr, L.Qty)
bottomLineToQty b = case b of
  L.Zero -> Nothing
  L.NonZero (L.Column dc q) -> Just (dc, q)

sumBottomLines :: [L.BottomLine] -> Maybe L.Qty
sumBottomLines ls = case catMaybes . map bottomLineToQty $ ls of
  [] -> Nothing
  x:xs -> Just $ foldl (\a b -> L.add a (snd b)) (snd x) xs

bottomLineToPercent
  :: L.Qty
  -- ^ Sum of all All BottomLines in this level
  -> L.BottomLine
  -- ^ This BottomLine
  -> Maybe KP.Percent
bottomLineToPercent tot bl = fmap f . bottomLineToQty $ bl
  where
    f (dc, q) = KP.Percent dc (L.divide q tot)


-- | Converts rows for a percentage report.
rowsPct
  :: L.To
  -- ^ To commodity
  -> E.Forest (L.SubAccount, Maybe KP.Percent)
  -> [KP.Row]
rowsPct to frt = first:rest
  where
    first = KP.ROneCol $ KP.OneColRow 0 desc
    desc = "All amounts reported in percents in commodity: "
           <> (L.unCommodity . L.unTo $ to)
    rest = map mainRowPct
         . concatMap E.flatten
         . map U.labelLevels
         $ frt

-- | Converts the balance data in preparation for screen rendering.
rows :: ForestAndBL -> ([K.Row], L.To)
rows (ForestAndBL f tot to) = (first:second:rest, to)
  where
    first = K.ROneCol $ K.OneColRow 0 desc
    desc = X.pack "All amounts reported in commodity: "
           `X.append` (L.unCommodity
                       . L.unTo
                       $ to)
    second = K.RMain $ K.MainRow 0 (X.pack "Total") tot
    rest = map mainRow
           . concatMap E.flatten
           . map U.labelLevels
           $ f


mainRowPct :: (Int, (L.SubAccount, Maybe KP.Percent)) -> KP.Row
mainRowPct (l, (a, p)) = KP.RMain $ KP.MainRow l (L.text a) p

mainRow :: (Int, (L.SubAccount, L.BottomLine)) -> K.Row
mainRow (l, (a, b)) = K.RMain $ K.MainRow l x b
  where
    x = L.text a

-- | The function for the Convert report. Use this function if you are
-- setting the options from a program (as opposed to parsing them in
-- from the command line.) Will fail if the balance conversions fail.
report
  :: Opts
  -> [L.PricePoint]
  -> [(a, L.Posting)]
  -> Ex.Exceptional X.Text [Rb.Chunk]
report os@(Opts eiFmt _ _ tgt _ txtFormats) ps bs = do
  fstBl <- sumConvertSort os ps bs
  return $ case eiFmt of
    Left getFmt ->
      let (rs, L.To cy) = rows fstBl
          fmt q = getFmt (L.Amount q cy)
      in K.rowsToChunks txtFormats fmt rs
    Right rnd ->
      let frt = forestToPercents (_tbForest fstBl)
          rws = rowsPct tgt frt
      in KP.rowsToChunks txtFormats rnd rws



-- | Creates a report respecting the standard interface for reports
-- whose options are parsed in from the command line.
cmdLineReport
  :: O.DefaultOpts
  -> I.Report
cmdLineReport o rt = (help o, mkMode)
  where
    mkMode _ _ chgrs _ fsf = MA.modeHelp
      "convert"
      (const (help o))
      (return . process rt chgrs o fsf)
      (map (fmap Right) P.allOptSpecs)
      MA.Intersperse
      (return . Left)


process
  :: S.Runtime
  -> Scheme.Changers
  -> O.DefaultOpts
  -> ([L.Transaction] -> [(Ly.LibertyMeta, L.Posting)])
  -> [Either String (P.Opts -> P.Opts)]
  -> I.ArgsAndReport
process rt chgrs defaultOpts fsf ls =
  let (posArgs, parsed) = Ei.partitionEithers ls
      op' = foldl (flip (.)) id parsed (O.toParserOpts defaultOpts rt)
      noDefault = X.pack "no default price found"
      f = fromParsedOpts chgrs op'
      pr fmt ts pps = do
        rptOpts <- Ex.fromMaybe noDefault $ f pps fmt
        let boxes = fsf ts
        report rptOpts pps boxes
  in (posArgs, pr)


-- | Sums the balances from the bottom to the top of the tree (so that
-- parent accounts have the sum of the balances of all their
-- children.) Then converts the commodities to a single commodity, and
-- sorts the accounts as requested. Fails if the conversion fails.
sumConvertSort
  :: Opts
  -> [L.PricePoint]
  -> [(a, L.Posting)]
  -> Ex.Exceptional X.Text ForestAndBL
sumConvertSort os ps bs = mkResult <$> convertedFrst <*> convertedTot
  where
    (Opts _ szb str tgt dt _) = os
    (topTot, unsorted) = U.balances szb bs
    (sorted, frstTot) = U.sumForest unsorted
    convertBal (a, bal) =
        (\bl -> (a, bl)) <$> convertBalance db dt tgt bal
    db = buildDb ps
    convertedFrst = mapM (Tvbl.mapM convertBal) sorted
    convertedTot = convertBalance db dt tgt (frstTot <> topTot)
    mkResult f t = ForestAndBL (U.sortForest str f) t tgt

-- | Determine the most frequent To commodity.
mostFrequent :: [L.PricePoint] -> Maybe L.To
mostFrequent = U.lastMode . map (L.to . L.price)


type DoReport
  = [L.PricePoint]
  -> (L.Amount L.Qty -> X.Text)
  -> (Maybe Opts)

-- | Get options for the report, depending on what options were parsed
-- from the command line. Fails if the user did not specify a
-- commodity and mostFrequent fails.
fromParsedOpts
  :: Scheme.Changers
  -> P.Opts
  -> DoReport
fromParsedOpts chgrs (P.Opts szb tgt dt so sb mayRnd) pps fmtAmt =
  let fmt = maybe (Left fmtAmt) Right mayRnd
  in case tgt of
       P.ManualTarget to ->
         Just $ Opts fmt szb (getSorter so sb) to dt chgrs
       P.AutoTarget ->
         case mostFrequent pps of
           Nothing -> Nothing
           Just to ->
             Just $ Opts fmt szb (getSorter so sb) to dt chgrs

-- | Returns a function usable to sort pairs of SubAccount and
-- BottomLine depending on how you want them sorted.
getSorter :: CP.SortOrder -> P.SortBy -> Sorter
getSorter o b = flipper f
  where
    flipper = case o of
      CP.Ascending -> id
      CP.Descending ->
        \g p1 p2 -> case g p1 p2 of
            LT -> GT
            GT -> LT
            EQ -> EQ
    f p1@(a1, _) p2@(a2, _) = case b of
      P.SortByName -> compare a1 a2
      P.SortByQty -> cmpBottomLine p1 p2

cmpBottomLine :: Sorter
cmpBottomLine (n1, bl1) (n2, bl2) =
  case (bl1, bl2) of
    (L.Zero, L.Zero) -> EQ
    (L.NonZero _, L.Zero) -> LT
    (L.Zero, L.NonZero _) -> GT
    (L.NonZero c1, L.NonZero c2) ->
      mconcat [dc, qt, na]
      where
        dc = case (Bal.colDrCr c1, Bal.colDrCr c2) of
          (L.Debit, L.Debit) -> EQ
          (L.Debit, L.Credit) -> LT
          (L.Credit, L.Debit) -> GT
          (L.Credit, L.Credit) -> EQ
        qt = compare (Bal.colQty c1) (Bal.colQty c2)
        na = compare n1 n2


------------------------------------------------------------
-- ## Help
------------------------------------------------------------
ifDefault :: Bool -> String
ifDefault b = if b then " (default)" else ""

help :: O.DefaultOpts -> String
help o = unlines $
  [ "convert"
  , "  Show account balances, after converting all amounts"
  , "  to a single commodity. Accepts ONLY the following options:"
  , ""
  , "--show-zero-balances"
  , "  Show balances that are zero"
    ++ ifDefault (CO.unShowZeroBalances . O.showZeroBalances $ o)
  , "--hide-zero-balances"
  , "  Hide balances that are zero"
    ++ ifDefault (not . CO.unShowZeroBalances . O.showZeroBalances $ o)
  , ""
  , "--commodity TARGET-COMMMODITY, -c TARGET-COMMODITY"
  , "  Convert all commodities to TARGET-COMMODITY."
  ] ++ case O.target o of
        P.ManualTarget (L.To cy) ->
          [ "  default: " ++ (X.unpack . L.unCommodity $ cy) ]
        _ -> []
    ++
  [ "--auto-commodity"
  , "  convert all commodities to the commodity that appears most"
  , "  often as the target commodity in your price data. If"
  , "  there is a tie, the price closest to the end of your list"
  , "  of prices is used."
    ++ case O.target o of
        P.AutoTarget -> " (default)"
        _ -> ""
  , ""
  , "--date DATE-TIME, -d DATE-TIME"
  , "  Convert prices as of the date and time given"
  , "  (by default, the current date and time is used.)"
  , ""
  , "--sort qty|name, -s qty|name"
  , "  Sort balances by sub-account name"
    ++ ifDefault (O.sortBy o == P.SortByName)
    ++ " or by quantity"
    ++ ifDefault (O.sortBy o == P.SortByQty)
  , "--order ascending|descending"
  , "  Sort order (default: "
       ++ if O.sortOrder o == CP.Ascending
          then "ascending" else "descending"
       ++ ")"
  , ""
  , "--percent, -%"
  , "  Show each account total as a percentage of the parent account"
  , "--round PLACES, -r PLACES"
  , "  Like --percent, but round to this many decimal places"
  , "  rather than the default 0 places"
  , ""
  , "--help, -h"
  , "  Show this help and exit"
  ]

