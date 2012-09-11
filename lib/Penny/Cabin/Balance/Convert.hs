-- | The Convert report.

module Penny.Cabin.Balance.Convert where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as C
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

data BalanceInfo = BalanceInfo {
  biAccount :: L.Account
  , biDrCr :: L.DrCr
  , biQty :: L.Qty
  }

balInfo ::
  L.PriceDb
  -> L.DateTime
  -> L.To
  -> L.Box a
  -> Ex.Exceptional X.Text BalanceInfo
balInfo db dt to b = Ex.mapExceptional e g ex
  where
    ex = L.convert db dt to am
    am = Q.amount . L.boxPostFam $ b
    fr = L.From  . L.commodity $ am
    e = convertError to fr
    g r = BalanceInfo ac dc (L.qty r)
    dc = Q.drCr . L.boxPostFam $ b
    ac = Q.account . L.boxPostFam $ b


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

