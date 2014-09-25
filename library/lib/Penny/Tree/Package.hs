-- | A Package associates the contents of a file with a name for those
-- contents (typically, this will be the name of the file.)

module Penny.Tree.Package where

import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Tree.File as File

data T = T
  { clxn :: Clxn.T
  , file :: File.T
  } deriving (Eq, Ord, Show)
