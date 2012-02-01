module Penny.Reports where

data Side = CommodityOnLeft | CommodityOnRight deriving Show
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving Show

data CommodityFmt =
  CommodityFmt { side :: Side
               , between :: SpaceBetween }
  deriving Show
