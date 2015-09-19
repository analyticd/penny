{-# LANGUAGE TemplateHaskell #-}

module Penny.Arrangement where

import Control.Lens

data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show)

type SpaceBetween = Bool

data Arrangement = Arrangement
  { _orient :: Orient
  , _spaceBetween :: SpaceBetween
  } deriving (Eq, Ord, Show)

makeLenses ''Arrangement
