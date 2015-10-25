{-# LANGUAGE TemplateHaskell #-}

module Penny.Columns.Env where

import Control.Lens (makeLenses)

import Penny.Clatch
import Penny.Popularity
import Penny.Colors

data Env = Env
  { _clatch :: Clatch
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env
