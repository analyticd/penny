module Penny.Reports.Grid where

import qualified Penny.Core.Wells as Wells
import Rainbox
import Data.Sequence (Seq)
import qualified Data.Foldable as F

report
  :: Background
  -- ^ Background color for even-numbered rows
  -> Background
  -- ^ Background color for odd-numbered rows

  -> Seq (Align Horiz, Background -> Wells.T -> [Bar])
  -- ^ There is one element in the sequence for each column.  Each
  -- column indicates its alignment and a function that, when applied
  -- to a 'Wells.T' and the default background color for this row,
  -- returns a list of 'Bar'.

  -> Seq Wells.T
  -- ^ Sequence of all 'Wells.T' in the report

  -> Box
report be bo cols wells = gridByCols . F.toList . fmap toCol $ cols
  where
    toCol (align, toBars) = zipWith mkRow colors . F.toList $ wells
      where
        colors = concat . repeat $ [be, bo]
        mkRow bk well = Cell (toBars bk well) align top bk
