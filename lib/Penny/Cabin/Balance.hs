-- | The Penny balance report
module Penny.Cabin.Balance (balanceReport, O.defaultOptions) where

import qualified Penny.Cabin.Balance.Parser as P
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Help as H
import qualified Penny.Cabin.Interface as I

balanceReport :: O.Options -> I.Report
balanceReport os = I.Report H.help pr where
  pr _ _ _ = do
    (f, os') <- P.parser os
    let cp = O.colorPref os'
        f' infos _ = return (f infos)
    return (f', cp)
