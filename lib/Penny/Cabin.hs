-- | Cabin - Penny reports
--
-- Cabin contains reports, or functions that take a list of postings
-- and return a formatted Text to display data in a human-readable
-- format.
module Penny.Cabin (allReportsWithDefaults, I.Report) where

import qualified Penny.Cabin.Balance as B
import qualified Penny.Cabin.Posts as P
import qualified Penny.Cabin.Interface as I

-- | Returns a list of all reports currently available in
-- Penny. Currently this list has just two reports: the Balance report
-- and the Postings report.
allReportsWithDefaults :: [I.Report]
allReportsWithDefaults =
  [ B.multiCommodity
  , B.convert
  , P.zincReport P.defaultOptions ]
