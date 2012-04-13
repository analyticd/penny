-- | The Penny balance report
module Penny.Cabin.Balance (balanceReport, O.defaultOptions,
                            defaultBalanceReport) where

import qualified Penny.Cabin.Balance.Parser as P
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Help as H
import qualified Penny.Cabin.Interface as I
import qualified Penny.Shield as S

balanceReport :: O.Options -> I.Report
balanceReport os = I.Report H.help pr where
  pr rt _ _ = do
    (f, _) <- P.parser rt os
    let f' infos _ = return (f infos)
    return f'

defaultBalanceReport :: S.Runtime -> I.Report
defaultBalanceReport rt = balanceReport (O.defaultOptions rt)
