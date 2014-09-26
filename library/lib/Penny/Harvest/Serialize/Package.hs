module Penny.Harvest.Serialize.Package where

import qualified Penny.Harvest.Serialize.Item as Item
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Harvest.Locate.Located as Located
import Data.Sequence (Seq)
import qualified Penny.Harvest.Serialize.State as State
import qualified Penny.Harvest.Locate.Package as Locate.Package
import qualified Data.Traversable as Tr
import Control.Applicative
import qualified Penny.Harvest.Locate.Item as Locate.Item

data T = T
  { clxn :: Clxn.T
  , items :: Seq (Located.T Item.T)
  } deriving (Eq, Ord, Show)



fwdLocal
  :: Seq (Located.T Locate.Item.T)
  -> ( Seq (State.T (State.T (State.T (Located.T Item.T))))
     , (Int, Int))
fwdLocal sq =
  let stateSeq = Tr.mapM Item.harvest sq
  in State.runState stateSeq (0,0)

revLocal
  :: (Int, Int)
  -> Seq (State.T (State.T (State.T (Located.T Item.T))))
  -> Seq (State.T (State.T (Located.T Item.T)))
revLocal start sq =
  let stateSeq = Tr.sequence sq
  in fst $ State.runState stateSeq start
