module Penny.Arrangement where


data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show)

type SpaceBetween = Bool

data Arrangement = Arrangement Orient SpaceBetween
  deriving (Eq, Ord, Show)
