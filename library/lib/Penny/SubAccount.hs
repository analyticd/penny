module Penny.SubAccount where

import Data.Text

data T = T { toText :: Text }
  deriving (Eq, Ord, Show)
