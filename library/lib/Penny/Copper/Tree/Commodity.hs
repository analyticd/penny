module Penny.Copper.Tree.Commodity
  ( CommodityChar
  , unCommodityChar
  , commodityChar
  , Commodity(..)
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

newtype CommodityChar = CommodityChar { unCommodityChar :: Char }
  deriving (Eq, Ord, Show)

commodityChar :: Char -> Maybe CommodityChar
commodityChar c
  | c /= '\n' && c /= '^' = Just $ CommodityChar c
  | otherwise = Nothing

data Commodity = Commodity Caret (Seq CommodityChar) Caret
  deriving (Eq, Ord, Show)
