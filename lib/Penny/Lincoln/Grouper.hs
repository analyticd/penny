module Penny.Lincoln.Grouper where

class Grouper a where
  groupChar :: a -> Char

data RPeriod
  = RPComma
  | RPThinSpace
  | RPSpace
  deriving (Eq, Ord, Show)

data RComma
  = RCPeriod
  | RCThinSpace
  | RCSpace
  deriving (Eq, Ord, Show)

instance Grouper RPeriod where
  groupChar a = case a of
    RPComma -> ','
    RPThinSpace -> '\x2009'
    RPSpace -> ' '

instance Grouper RComma where
  groupChar a = case a of
    RCPeriod -> '.'
    RCThinSpace -> '\x2009'
    RCSpace -> ' '
