module Penny.Preset where

import Control.Lens (set)
import Penny.Clatcher
import Penny.Scheme
import Penny.Stream

-- | Sends output to @less@ in color, using a light background.

coless :: Clatcher r l
coless
  = set output (stream toLess)
  . set colors light
  $ mempty

-- | Sends output to @less@ in color.
