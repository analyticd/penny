module Penny.Serial where

data T = T
  { forward :: Int
  , backward :: Int
  } deriving (Eq, Ord, Show)
