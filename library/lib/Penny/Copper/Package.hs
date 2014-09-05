module Penny.Copper.Package where

import Penny.Common
import Data.Sequence
import Penny.Copper.Tree.File

data Package = Package Clxn File
  deriving (Eq, Ord, Show)

newtype Packages = Packages { unPackages :: Seq Package }
  deriving (Eq, Ord, Show)
