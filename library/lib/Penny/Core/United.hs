module Penny.Core.United where

data T a = T
  { topLine :: a
  , posting :: a
  } deriving (Eq, Ord, Show)
