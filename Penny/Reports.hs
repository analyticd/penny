module Penny.Reports where

data Side = CommodityOnLeft | CommodityOnRight
data SpaceBetween = SpaceBetween | NoSpaceBetween

data CommodityFmt =
  CommodityFmt { side :: Side
               , between :: SpaceBetween }
