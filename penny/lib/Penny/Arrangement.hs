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

data Arrangement = Arrangement
  { _orient :: Orient
  , _spaceBetween :: SpaceBetween
  } deriving (Eq, Ord, Show, Generic, PrettyVal)

makeLenses ''Arrangement

-- | Commodity on left, no space between.
arrangeDollars :: Arrangement
arrangeDollars = Arrangement CommodityOnLeft False

-- | Commodity on right, space between; typical for stock shares.
arrangeShares :: Arrangement
arrangeShares = Arrangement CommodityOnRight True
