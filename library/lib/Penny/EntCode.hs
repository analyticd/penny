module Penny.EntCode where

-- | Different errors that may arise when processing a single 'T.Trio'
-- for conversion to an 'Ent'.
data T
  = SCWrongSide
  | SWrongSide
  | CommodityNotFound
  | NoCommoditiesInBalance
  | MultipleCommoditiesInBalance
  | QQtyTooBig
  deriving (Eq, Ord, Show)

