module Penny.Flag where

import Data.Text

newtype T = T { toText :: Text }
  deriving (Eq, Ord, Show)
