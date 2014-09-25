module Penny.Harvest.Locate.Packages where

import qualified Penny.Harvest.Locate.Package as Package
import qualified Penny.Tree.Packages as Tree.Packages
import Data.Sequence (Seq)

data T = T { toSeq :: Seq Package.T }
  deriving (Eq, Ord, Show)

harvest :: Tree.Packages.T -> T
harvest (Tree.Packages.T sq) = T (fmap Package.harvest sq)
