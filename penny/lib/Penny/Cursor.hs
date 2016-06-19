{-# LANGUAGE TemplateHaskell #-}
-- | Locations of items.

module Penny.Cursor where

import qualified Control.Lens as Lens
import Data.Text (Text)
import qualified Pinchot

data Cursor = Cursor
  { _filename :: Text
  , _loc :: Pinchot.Loc
  } deriving (Eq, Ord, Show)

Lens.makeLenses ''Cursor
