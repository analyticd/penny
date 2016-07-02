{-# LANGUAGE TemplateHaskell #-}

module Penny.Columns.Env where

import Control.Lens (makeLenses)

import Penny.Clatch
import Penny.Popularity
import Penny.Colors
import Penny.FileLoc

data Env = Env
  { _clatch :: Clatch (Maybe FileLoc)
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env
