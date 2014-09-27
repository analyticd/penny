module Penny.Core.Hours
  ( T
  , toInt
  , fromInt
  , zero
  ) where

data T = T { toInt :: Int }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe T
fromInt i
  | i < 0 = Nothing
  | i > 23 = Nothing
  | otherwise = Just . T $ i

zero :: T
zero = T 0
