{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Penny.Arrangement where

import Control.Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show, Generic, PrettyVal)

type SpaceBetween = Bool

space :: SpaceBetween
space = True

noSpace :: SpaceBetween
noSpace = False

data Arrangement = Arrangement
  { _orient :: Orient
  , _spaceBetween :: SpaceBetween
  } deriving (Eq, Ord, Show, Generic, PrettyVal)

makeLenses ''Arrangement
