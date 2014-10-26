module Penny.Reports.Grid.Columns where

import qualified Penny.Core.Wells as Wells
import Rainbox
import Rainbow
import qualified Penny.Reports.Foreground as Foreground

-- | The YYYY-MM-DD date only.
date
  :: Foreground.T
  -> (Align Horiz, Background -> Wells.T -> [Bar])
date = undefined
