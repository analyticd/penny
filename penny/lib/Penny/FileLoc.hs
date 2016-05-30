{-# LANGUAGE TemplateHaskell #-}
module Penny.FileLoc where

import qualified Control.Lens as Lens
import Data.Text (Text)
import Pinchot (Loc)
import qualified Pinchot

data FileLoc = FileLoc
  { _loc :: Loc
  , _filename :: Text
  }

Lens.makeLenses ''FileLoc

line :: Lens.Lens' FileLoc Int
line = loc . Pinchot.line

col :: Lens.Lens' FileLoc Int
col = loc . Pinchot.col

pos :: Lens.Lens' FileLoc Int
pos = loc . Pinchot.pos
