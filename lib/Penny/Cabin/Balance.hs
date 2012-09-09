-- | The Penny balance report
module Penny.Cabin.Balance where

import qualified Penny.Cabin.Balance.MultiCommodity as MC
import qualified Penny.Cabin.Interface as I

-- | The default multi-commodity balance report.
multiCommodity :: I.Report
multiCommodity = MC.defaultReport
