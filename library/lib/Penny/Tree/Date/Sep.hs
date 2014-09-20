module Penny.Tree.Date.Sep where

import qualified Penny.Tree.Hyphen as Hyphen
import qualified Penny.Tree.Solidus as Solidus

newtype T = T (Either Hyphen.T Solidus.T)
  deriving (Eq, Ord, Show)
