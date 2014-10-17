module Penny.Harvest.Serialized.Item where

import qualified Penny.Tree.Memo.Transaction as MemoT
import qualified Penny.Tree.Memo.Posting as MemoP
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Tree.Posting as Posting
import qualified Penny.Core.Serial.Global as SerialG
import qualified Penny.Core.Serial.Local as SerialL
import qualified Penny.Harvest.Zoned.Item as Locate.Item
import qualified Penny.Harvest.Serialized.State as State
import qualified Penny.Core.Serial as Serial
import qualified Penny.Harvest.Zoned.Located as Located

data T
  = MemoT MemoT.T
  | TopLine TopLine.T SerialL.T SerialG.T
  | Posting Posting.T SerialL.T SerialG.T
  | MemoP MemoP.T
  deriving (Eq, Ord, Show)

-- | Applied to a "Penny.Harvest.Locate.Item", returns a nested
-- stateful computation.  From outermost to innermost, these stateful
-- computations compute:
--
-- * forward serials, local
-- * reverse serials, local
-- * forward serials, global
-- * reverse serials, global
harvest
  :: Located.T Locate.Item.T
  -> State.T (State.T (State.T (State.T (Located.T T))))

harvest ((Located.T loc (Locate.Item.T0 memo)))
  = return . return . return . return . Located.T loc . MemoT $ memo

harvest ((Located.T loc (Locate.Item.T1 tl))) = do
  fwdTopLocal <- State.topLineFwd
  return $ do
    revTopLocal <- State.topLineRev
    return $ do
      fwdTopGlobal <- State.topLineFwd
      return $ do
        revTopGlobal <- State.topLineRev
        return . Located.T loc $ TopLine tl
          (SerialL.T (Serial.T fwdTopLocal revTopLocal))
          (SerialG.T (Serial.T fwdTopGlobal revTopGlobal))

harvest ((Located.T loc (Locate.Item.T2 ps))) = do
  fwdPstLocal <- State.topLineFwd
  return $ do
    revPstLocal <- State.topLineRev
    return $ do
      fwdPstGlobal <- State.topLineFwd
      return $ do
        revPstGlobal <- State.topLineRev
        return . Located.T loc $ Posting ps
          (SerialL.T (Serial.T fwdPstLocal revPstLocal))
          (SerialG.T (Serial.T fwdPstGlobal revPstGlobal))

harvest ((Located.T loc (Locate.Item.T3 memo)))
  = return . return . return . return . Located.T loc . MemoP $ memo
