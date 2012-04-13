-- | The Penny balance report
module Penny.Cabin.Balance (balanceReport, O.defaultOptions,
                            O.Options(..),
                            defaultBalanceReport) where

import qualified Penny.Cabin.Balance.Parser as P
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Help as H
import qualified Penny.Cabin.Interface as I
import qualified Penny.Shield as S

balanceReport :: (S.Runtime -> O.Options) -> I.Report
balanceReport getOpts = I.Report H.help pr where
  pr rt _ _ = do
    (f, _) <- P.parser rt (getOpts rt)
    let f' infos _ = return (f infos)
    return f'

defaultBalanceReport :: I.Report
defaultBalanceReport = balanceReport O.defaultOptions
