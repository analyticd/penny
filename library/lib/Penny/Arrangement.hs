module Penny.Arrangement where

import qualified Penny.Orient as O
import qualified Penny.SpaceBetween as S

data T = T
  { orient :: O.T
  , spaceBetween :: S.T
  } deriving (Eq, Ord, Show)
