-- | Penny balance reports. Currently there are two balance reports:
-- the MultiCommodity report, which cannot convert commodities and
-- which therefore might show more than one commodity in a single
-- report, and the Convert report, which uses price data in the Penny
-- file to convert all commodities to a single commodity. The Convert
-- report always displays only one commodity per account and this one
-- commodity for the whole report.
module Penny.Cabin.Balance where

import qualified Penny.Cabin.Balance.MultiCommodity as MC
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Balance.Convert as C
import qualified Penny.Cabin.Balance.Convert.Options as ConvOpts
import qualified Penny.Lincoln as L
import qualified Data.Text as X

-- | The default multi-commodity balance report.
multiCommodity :: (L.Amount L.Qty -> X.Text) -> I.Report
multiCommodity = MC.defaultReport

-- | The default converting balance report.
convert :: (L.Amount L.Qty -> X.Text) -> I.Report
convert fmt = C.cmdLineReport fmt ConvOpts.defaultOptions
