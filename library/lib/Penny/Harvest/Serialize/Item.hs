module Penny.Harvest.Serialize.Item where

import qualified Penny.Tree.Memo.Transaction as MemoT
import qualified Penny.Tree.Memo.Posting as MemoP
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Tree.Posting as Posting
import qualified Penny.Core.Serial.Global as SerialG
import qualified Penny.Core.Serial.Local as SerialL
import qualified Penny.Harvest.Serialize.State as State
import qualified Penny.Harvest.Locate.Item as Locate.Item
import qualified Control.Monad.Trans.State as St
import qualified Penny.Core.Serial as Serial

data T
  = T0 MemoT.T
  | T1 TopLine.T SerialL.T SerialG.T
  | T2 Posting.T SerialL.T SerialG.T
  | T3 MemoP.T
  deriving (Eq, Ord, Show)

-- | Applied to a "Penny.Harvest.Locate.Item", returns a nested
-- stateful computation.  The outer state computation holds
-- information on the forward serials.  The inner state computation
-- holds information on the reverse serials.  When running the state
-- computations, first the outer computation is run to compute the
-- forward serials; then, the resulting inner computations are run to
-- obtain the reverse serials.
harvest :: Locate.Item.T -> St.State State.T (St.State State.T T)

harvest (Locate.Item.T0 memo) = return (return $ T0 memo)

harvest (Locate.Item.T1 tl) = do
  (State.T fwdLocalTop _ fwdGlobalTop _) <- St.get
  St.modify State.incrTopLine
  return $ do
    (State.T backLocalTop _ backGlobalTop _) <- St.get
    St.modify State.decrTopLine
    return $ T1 tl (SerialL.T (Serial.T fwdLocalTop backLocalTop))
                   (SerialG.T (Serial.T fwdGlobalTop backGlobalTop))

harvest (Locate.Item.T2 ps) = do
  State.T _ fwdLocalPstg _ fwdGlobalPstg <- St.get
  St.modify State.incrPosting
  return $ do
    State.T _ backLocalPstg _ backGlobalPstg <- St.get
    St.modify State.decrPosting
    return $ T2 ps (SerialL.T (Serial.T fwdLocalPstg backLocalPstg))
                   (SerialG.T (Serial.T fwdGlobalPstg backGlobalPstg))

harvest (Locate.Item.T3 m) = return (return $ T3 m)
