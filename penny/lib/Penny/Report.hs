module Penny.Report where

import Data.Sequence (Seq)
import Data.Text (Text)
import Rainbow (Chunk)

import Penny.Clatch
import Penny.Colors
import Penny.Cursor
import Penny.Popularity


-- | Things that can produce a report.
-- For an example instance, see 'Penny.Register.Register'.
class Report a where
  printReport
    :: Seq a
    -- ^ The report to print
    -> Colors
    -> History
    -> Seq (Clatch (Maybe Cursor))
    -- ^ Sequence of 'Clatch' for which to prepare a report
    -> Seq (Chunk Text)
    -- ^ The resulting report
