-- | The Convert report.

module Penny.Cabin.Balance.Convert where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Data.Text as X

data Opts = Opts {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  , sorter :: Sorter
  , target :: L.Commodity
  , dateTime :: L.DateTime
  }

type Sorter =
  L.Commodity
  -> (L.DrCr, L.Qty)
  -> (L.DrCr, L.Qty)
  -> Ordering

convertOne ::
  L.PriceDb
  -> L.DateTime
  -> L.To
  -> (L.Commodity, L.BottomLine)
  -> Ex.Exceptional X.Text L.BottomLine
convertOne db dt to (cty, bl) =
  case bl of
    L.Zero -> return L.Zero
    L.NonZero (L.Column dc qt) -> Ex.mapExceptional e g ex
      where
        ex = L.convert db dt to am
        am = L.Amount qt cty
        e = convertError to (L.From cty)
        g r = L.NonZero (L.Column dc r)

convertError ::
  L.To
  -> L.From
  -> L.PriceDbError
  -> X.Text
convertError (L.To to) (L.From fr) e =
  let fromErr = L.text (L.Delimited (X.singleton ':')
                        (Fdbl.toList . L.unCommodity $ fr))
      toErr = L.text (L.Delimited (X.singleton ':')
                      (Fdbl.toList . L.unCommodity $ to))
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


buildDb :: [L.PricePoint] -> L.PriceDb
buildDb = foldl f L.emptyDb where
  f db pb = L.addPrice db pb

report ::
  Opts
  -> [L.PricePoint]
  -> [L.Box a]
  -> Ex.Exceptional X.Text [Chunk.Chunk]
report os ps bs = undefined

