-- after Ida B. Wells
module Penny.Core.Wells where

import qualified Penny.Core.Serial as Serial
import qualified Penny.Core.Fortune as Fortune
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

filterFortunes
  :: (Fortune.T -> Bool)
  -> Seq Fortune.T
  -> Seq T
filterFortunes p =
  (\fs -> Seq.zipWith T fs (Serial.serials (Seq.length fs)))
  . Seq.filter p
