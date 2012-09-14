-- | The Penny balance report
module Penny.Cabin.Balance where

import qualified Penny.Cabin.Balance.MultiCommodity as MC
import qualified Penny.Cabin.Interface as I
import qualified Penny.Copper as Cop
import qualified Penny.Cabin.Balance.Convert as C
import qualified Penny.Cabin.Balance.Convert.Options as ConvOpts

-- | The default multi-commodity balance report.
multiCommodity :: I.Report
multiCommodity = MC.defaultReport

-- | The default converting balance report.
convert :: Cop.DefaultTimeZone
           -> I.Report
convert dtz = C.cmdLineReport dtz ConvOpts.defaultOptions
