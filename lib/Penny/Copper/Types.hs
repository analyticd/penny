module Penny.Copper.Types where

import qualified Data.Text as X

newtype Comment = Comment { unComment :: X.Text }
  deriving Show
