{-# LANGUAGE TemplateHaskell #-}

module Penny.Table.Env where

import Control.Lens (makeLenses)

import Penny.Clatch.Types
import Penny.Colors
import Penny.Cursor
import Penny.Popularity

data Env = Env
  { _clatch :: Clatch (Maybe Cursor)
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env
