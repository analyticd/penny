module Penny.Harvest.Serialize.Packages where

import qualified Penny.Harvest.Serialize.Package as Package
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Penny.Harvest.Serialize.State as State
import qualified Penny.Harvest.Locate.Packages as Locate.Packages
import qualified Control.Monad.Trans.State as St

data T = T { toSeq :: Seq Package.T }
  deriving (Eq, Ord, Show)


harvest :: Locate.Packages.T -> T
harvest = undefined

