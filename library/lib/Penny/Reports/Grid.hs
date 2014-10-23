module Penny.Reports.Grid where

import qualified Penny.Core.Wells as Wells
import Rainbox.Box
import Data.Sequence (Seq)

report
  :: Seq (Seq Wells.T -> Wells.T -> Box)
  -- ^ There is one element in the sequence for each column.  Each
  -- column is a function that, when applied to all 'Wells.T' and to
  -- an individual 'Wells.T', returns a single 'Box'.
  -> Seq Wells.T
  -- ^ Sequence of all 'Wells.T' in the report
  -> Seq (Seq Box)
  -- ^ The outer 'Seq' has an element for each column.  Each inner
  -- 'Seq' is the length of the number of 'Wells.T' in the report.
report cols wells = fmap toBoxes . fmap toFunc $ cols
  where
    toBoxes wellsToBox = fmap (wellsToBox $) wells
    toFunc = ($ wells)
