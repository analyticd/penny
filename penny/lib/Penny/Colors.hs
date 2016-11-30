{-# LANGUAGE TemplateHaskell #-}

module Penny.Colors where

import Control.Lens (makeLenses)
import Data.Monoid ((<>))
import Rainbow (Radiant, cyan, magenta, yellow, red, only256, color256)

-- | Load data into this record to make a color scheme that has
-- different colors for debits and credits, with an alternating
-- background for odd- and even-numbered postings.
data Colors = Colors
  { _debit :: Radiant
  , _credit :: Radiant
  , _neutral :: Radiant
  , _nonLinear :: Radiant
  , _notice :: Radiant
  , _oddBackground :: Radiant
  , _evenBackground :: Radiant
  } deriving (Eq, Ord, Show)

makeLenses ''Colors

instance Monoid Colors where
  mempty = Colors
    { _debit = mempty
    , _credit = mempty
    , _neutral = mempty
    , _nonLinear = mempty
    , _notice = mempty
    , _oddBackground = mempty
    , _evenBackground = mempty
    }

  mappend (Colors x0 x1 x2 x3 x4 x5 x6) (Colors y0 y1 y2 y3 y4 y5 y6)
    = Colors (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3)
             (x4 <> y4) (x5 <> y5) (x6 <> y6)


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
