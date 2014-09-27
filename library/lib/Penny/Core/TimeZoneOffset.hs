module Penny.Core.TimeZoneOffset
  ( T
  , toInt
  , fromInt
  , zero
  ) where

data T = T { toInt :: Int }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe T
fromInt i
  | abs i > 840 = Nothing
  | otherwise = Just . T $ i

zero :: T
zero = T 0
