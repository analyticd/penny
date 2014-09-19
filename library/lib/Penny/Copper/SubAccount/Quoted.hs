module Penny.Copper.SubAccount.Quoted where

import qualified Penny.Copper.SubAccount.Quoted.Char as Char
import Data.Sequence (Seq)

data T = T { toSeq :: Seq Char.T }
  deriving (Eq, Ord, Show)
