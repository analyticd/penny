module Penny.Tree.Time.Time4 where

import qualified Penny.Tree.Time.Zone as Zone
import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Brace.Close as Close

data T
  = Zone Zone.T
  | Space Space.T Zone.T
  | Close Close.T
  deriving (Eq, Ord, Show)
