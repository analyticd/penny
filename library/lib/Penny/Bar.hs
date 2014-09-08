-- | A single line of text.  Ordinarily a Bar does not include a
-- newline; however, there is nothing to enforce this convention.

module Penny.Bar where

import Data.Text

newtype T = T { toText :: Text }
  deriving (Eq, Ord, Show)
