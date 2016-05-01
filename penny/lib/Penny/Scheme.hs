-- | Color schemes.

module Penny.Scheme where

import Penny.Colors
import Data.Monoid ((<>))
import Rainbow (cyan, magenta, yellow, red, only256, color256)

-- | Colors for use on a terminal with a dark background.

dark :: Colors
dark = Colors
  { _debit = cyan
  , _credit = magenta
  , _neutral = yellow
  , _nonLinear = mempty
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = mempty <> only256 (color256 236)
  }

-- | Colors for use on a terminal with a light background.

light :: Colors
light = Colors
  { _debit = cyan
  , _credit = magenta
  , _neutral = yellow
  , _nonLinear = mempty
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = mempty <> only256 (color256 255)
  }
