module Penny.Copper.SubAccount.Quoted.Next where

import qualified Penny.Copper.SubAccount.Quoted.Char as Char
import qualified Penny.Copper.Colon as Colon
import Data.Sequence (Seq)

data T = T Colon.T (Seq Char.T)
  deriving (Eq, Ord, Show)
