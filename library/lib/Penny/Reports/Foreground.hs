module Penny.Reports.Foreground where

import Rainbow

data T = T
  { c8 :: Color8
  , c256 :: Color8 Color256
  } deriving (Eq, Ord, Show)
