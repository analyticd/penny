-- after Ida B. Wells
module Penny.Core.Wells where

import qualified Penny.Core.Serial as Serial
import qualified Penny.Core.Fortune as Fortune
import qualified Data.Foldable as F
import qualified Penny.Core.TopLine as TopLine

-- | A 'Penny.Core.Fortune.T', combined with some additional serials
-- and a running balance.  This type is used only for postings that
-- are visible in the report.

data T = T
  { fortune :: Fortune.T

  , postReportFilter :: Serial.T
  -- ^ After postings are filtered to determine which postings are in
  -- the report but are not visible, the visible postings are assigned
  -- this serial.  Postings that are not visible still count toward
  -- the report's running balance.
  } deriving (Eq, Ord, Show)

topLine :: T -> TopLine.T
topLine = Fortune.topLine . fortune

-- | Create a list of 'T' from a list of 'Fortune.T'.  Presumably the
-- list of 'Fortune.T' has already been filtered.  The input list must
-- be finite.

fromFortuneList :: [Fortune.T] -> [T]
fromFortuneList fs =
  zipWith T fs (F.toList $ Serial.serials (length fs))
