module Penny.Harvest.Serialize.Package where

import qualified Penny.Harvest.Serialize.Item as Item
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Harvest.Locate.Located as Located
import Data.Sequence (Seq)
import qualified Penny.Harvest.Serialize.State as State
import qualified Control.Monad.Trans.State as St
import qualified Penny.Harvest.Locate.Package as Locate.Package
import qualified Data.Traversable as Tr
import Control.Applicative

data T = T
  { clxn :: Clxn.T
  , items :: Seq (Located.T Item.T)
  } deriving (Eq, Ord, Show)

harvest :: Locate.Package.T -> St.State State.T (St.State State.T T)
harvest (Locate.Package.T clx is) = do
  states <- Tr.mapM (Tr.traverse Item.harvest) is
  let items = Tr.sequence (fmap Tr.traverse states)
  undefined

