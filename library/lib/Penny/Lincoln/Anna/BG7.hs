module Penny.Lincoln.Anna.BG7 where

import qualified Penny.Lincoln.Anna.Nodecs3 as Nodecs3
import qualified Penny.Zeroes as Zeroes

data T r
  = LeadZeroes Zeroes.T (Either (r, T r) (Nodecs3.T r))
  | LeadNovem (Nodecs3.T r)
  deriving (Eq, Ord, Show)
