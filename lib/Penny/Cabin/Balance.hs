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
balanceReport getOpts = I.Report H.help "balance" pr
  where
    pr rt = do
      fn <- P.parser rt
      let opInit = getOpts rt
          f _ _ bs ps = fn opInit bs ps
      return f

defaultBalanceReport :: I.Report
defaultBalanceReport = balanceReport O.defaultOptions
