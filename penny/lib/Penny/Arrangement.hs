{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Penny.Arrangement where

import Control.Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show, Generic)

instance PrettyVal Orient

type SpaceBetween = Bool

data Arrangement = Arrangement
  { _orient :: Orient
  , _spaceBetween :: SpaceBetween
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Arrangement

makeLenses ''Arrangement
