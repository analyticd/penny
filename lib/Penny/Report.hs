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
    :: Ledger l
    => a l
    -- ^ The report to print
    -> (Amount -> NilOrBrimScalarAnyRadix)
    -- ^ Represents calculated amounts
    -> Seq (Clatch l)
    -- ^ Sequence of 'Clatch' for which to prepare a report
    -> l (Seq (Chunk Text))
    -- ^ The resulting report
