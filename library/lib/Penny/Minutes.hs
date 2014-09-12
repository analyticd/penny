module Penny.Minutes
  ( T
  , toInt
  , fromInt
  ) where

data T = T { toInt :: Int }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe T
fromInt i
  | i < 0 = Nothing
  | i > 59 = Nothing
  | otherwise = Just . T $ i
