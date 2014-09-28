module Penny.Tree.Ingot.Error where

data T
  = CurrencyWithNilRepresentation
  | NilRepresentation
  | DuplicateCommodities
  | SideWithZeroRepresentation
  deriving (Eq, Ord, Show)
