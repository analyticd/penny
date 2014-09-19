module Penny.Copper.SubAccount.Quoted.First where

import qualified Penny.Copper.SubAccount.Quoted.Char as Char
import Data.Sequence (Seq)

data T = T (Seq Char.T)
  deriving (Eq, Ord, Show)
