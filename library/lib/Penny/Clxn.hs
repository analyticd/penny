module Penny.Clxn where

import Data.Text

newtype T = T { toText :: Text }
  deriving (Eq, Ord, Show)
