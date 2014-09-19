module Penny.Copper.SubAccount.Unquoted.Next where

import qualified Penny.Copper.SubAccount.Unquoted.Char.Next as NextC
import Data.Sequence (Seq)
import qualified Penny.Copper.Colon as Colon

data T = T
  { colon :: Colon.T
  , first :: NextC.T
  , rest :: Seq NextC.T
  } deriving (Eq, Ord, Show)
