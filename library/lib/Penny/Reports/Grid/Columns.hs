module Penny.Reports.Grid.Columns where

import qualified Penny.Core.Wells as Wells
import Rainbox
import Rainbow

-- | The YYYY-MM-DD date only.
date
  :: Both
  -- ^ Foreground colors
  -> (Align Horiz, Background -> Wells.T -> [Bar])
date fg = (left, fn)
  where
    fn bg wells = [Bar $ txt <> fore fg, back bg]
      where
        topLine
          = Bundle.topLine
          . Fortune.bundle
          . Wells.fortune
          $ wells
        txt = undefined
