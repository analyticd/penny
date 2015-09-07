module Penny.Report where

import Data.Sequence (Seq)
import Data.Text (Text)
import Penny.Clatch
import Penny.Popularity
import Rainbow


-- | Things that can produce a report.
-- For an example instance, see 'Penny.Register.Register'.
class Report a where
  printReport
    :: a
    -- ^ The report to print
    -> History
    -> Seq Clatch
    -- ^ Sequence of 'Clatch' for which to prepare a report
    -> Seq (Chunk Text)
    -- ^ The resulting report
