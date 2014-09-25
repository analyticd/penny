module Penny.Tree.Packages where

import qualified Penny.Tree.Package as Package
import qualified Data.Sequence as S

newtype T = T { toSeq :: S.Seq Package.T }
  deriving (Eq, Ord, Show)
