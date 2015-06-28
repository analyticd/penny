module Penny.Report where

import Data.Sequence (Seq)
import Data.Text (Text)
import Penny.Amount
import Penny.Clatch
import Penny.Ledger
import Penny.Representation
import Rainbow


-- | Things that can produce a report.
-- For an example instance, see 'Penny.Register.Register'.
class Report a where
  printReport
    :: a
    -- ^ The report to print
    -> (Amount -> NilOrBrimScalarAnyRadix)
    -- ^ Represents calculated amounts
    -> Seq Clatch
    -- ^ Sequence of 'Clatch' for which to prepare a report
    -> Ledger (Seq (Chunk Text))
    -- ^ The resulting report
