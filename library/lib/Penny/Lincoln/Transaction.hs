module Penny.Lincoln.Transaction where

import Penny.Lincoln.Ents
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Data.Map (Map)
import Data.Sequence (Seq, viewl, ViewL(..))
import Data.Monoid
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import qualified Data.Map as M

newtype TopLine = TopLine (Map Label Payload)
  deriving (Eq, Ord, Show)

data PstgMeta = PstgMeta (Map Label Payload) Trio
  deriving (Eq, Ord, Show)

-- | A balanced set of postings, along with common metadata for all
-- the postings (often called the /top line data/, as it appears on
-- the top line of a checkbook register transaction.)
data Transaction
  = Transaction TopLine (Balanced PstgMeta)
  -- ^ @Transaction a b@, where
  --
  -- @a@ is the top line data
  --
  -- @b@ is the 'Balanced' set of postings; each of these may carry
  -- its own metadata.
  deriving (Eq, Ord, Show)

data TransactionError
  = BadTrio PstgMeta TrioError
  -- ^ A particular 'Trio' could not create an 'Ent'.  Its
  -- accompanying metadata is also returned.
  | ImbalancedTransaction ImbalancedError
  -- ^ Each 'Trio' is satisfactory, but altogether they are not balanced.
  deriving (Eq, Ord, Show)

-- | Creates new 'Transaction'.  Fails if the input data is not
-- balanced or if one of the 'Trio' causes an error.
transaction
  :: TopLine
  -- ^ Top line data
  -> Seq PstgMeta
  -- ^ Each posting
  -> Either TransactionError Transaction
transaction topLine sqnce = makeEnts >>= makeTxn
  where
    makeEnts = go mempty sqnce
      where
        go soFar sq = case viewl sq of
          EmptyL -> return soFar
          pm@(PstgMeta _ tri) :< xs -> case appendTrio soFar tri of
            Left e -> Left $ (BadTrio pm) e
            Right fn -> go (fn pm) xs

    makeTxn ents = case entsToBalanced ents of
      Left e -> Left $ ImbalancedTransaction e
      Right g -> Right $ Transaction topLine g

-- | A single posting, bundled with its sibling postings and with top
-- line metadata.
data Bundle = Bundle TopLine (View PstgMeta)
  deriving (Eq, Ord, Show)

transactionToBundles :: Transaction -> Seq Bundle
transactionToBundles (Transaction tl bal) =
  fmap (Bundle tl) $ allViews bal

bundleToTransaction :: Bundle -> Transaction
bundleToTransaction (Bundle tl v) = Transaction tl (viewToBalanced v)

nextBundle :: Bundle -> Maybe Bundle
nextBundle (Bundle tl v) = fmap (Bundle tl) $ moveRight v

prevBundle :: Bundle -> Maybe Bundle
prevBundle (Bundle tl v) = fmap (Bundle tl) $ moveLeft v

siblingBundles :: Bundle -> Seq Bundle
siblingBundles (Bundle tl v) = fmap (Bundle tl) $ siblingViews v

-- | Adds serials for 'fwdGlobalSerial', 'revGlobalSerial',
-- 'fwdFileSerial', and 'revFileSerial'.  Any duplicate serials
-- already existing are overwritten.

addSerials
  :: Seq (Seq Transaction)
  -- ^ There is one input Seq for each file.  The inner Seq contains
  -- the transactions to which to assign serials.
  -> Seq (Seq Transaction)
addSerials = addGlobalSerials . fmap addFileSerials

-- | Adds global serials only.
addGlobalSerials
  :: Seq (Seq Transaction)
  -> Seq (Seq Transaction)
addGlobalSerials = addGlobalTxnSerials . addGlobalPstgSerials

-- | Adds global transaction serials.
addGlobalTxnSerials
  :: Seq (Seq Transaction)
  -> Seq (Seq Transaction)
addGlobalTxnSerials txns = flip evalState 0 $ assignFwd >>= assignRev
  where
    assignFwd = T.traverse assignOuter txns
      where
        assignOuter = T.traverse assignInner
          where
            assignInner = assignTxnFwd fwdTxnGlobalSerial

    assignRev txns' = T.traverse assignOuter txns'
      where
        assignOuter inner = T.traverse assignInner inner
          where
            assignInner = assignTxnRev revTxnGlobalSerial

-- | Adds global posting serials.
addGlobalPstgSerials
  :: Seq (Seq Transaction)
  -> Seq (Seq Transaction)
addGlobalPstgSerials txns = flip evalState 0 $ assignFwd >>= assignRev
  where
    assignFwd = T.traverse assignOuter txns
      where
        assignOuter = T.traverse assignInner
          where
            assignInner (Transaction tl bal) = fmap (Transaction tl)
              $ T.traverse assignPstg bal
              where
                assignPstg = assignPstgFwd fwdPstgGlobalSerial

    assignRev txns' = T.traverse assignOuter txns'
      where
        assignOuter = T.traverse assignInner
          where
            assignInner (Transaction tl bal) = fmap (Transaction tl)
              $ T.traverse assignPstg bal
              where
                assignPstg = assignPstgRev revPstgGlobalSerial

-- | Adds file serials only.

addFileSerials
  :: Seq Transaction
  -> Seq Transaction
addFileSerials
  = fmap addPstgIndexSerials
  . addFileTxnSerials
  . addFilePstgSerials

-- | Adds file posting serials.
addFileTxnSerials
  :: Seq Transaction
  -> Seq Transaction
addFileTxnSerials txns = flip evalState 0 $ assignFwd >>= assignRev
  where
    assignFwd = T.traverse assign txns
      where
        assign = assignTxnFwd fwdPstgFileSerial

    assignRev = T.traverse assign
      where
        assign = assignTxnRev revPstgFileSerial

addFilePstgSerials
  :: Seq Transaction
  -> Seq Transaction
addFilePstgSerials txns = flip evalState 0 $ assignFwd >>= assignRev
  where
    assignFwd = T.traverse assign txns
      where
        assign (Transaction tl bal) = fmap (Transaction tl)
          $ T.traverse innerAssign bal
          where
            innerAssign = assignPstgFwd fwdPstgFileSerial

    assignRev = T.traverse assign
      where
        assign (Transaction tl bal) = fmap (Transaction tl)
          $ T.traverse innerAssign bal
          where
            innerAssign = assignPstgRev revPstgFileSerial

addPstgIndexSerials :: Transaction -> Transaction
addPstgIndexSerials (Transaction tl bal) =
  Transaction tl . flip evalState 0 $ assignFwd >>= assignRev
  where
    assignFwd = T.traverse assign bal
      where
        assign = assignPstgFwd fwdPstgIndexSerial
    assignRev = T.traverse assign
      where
        assign = assignPstgRev revPstgIndexSerial

addTxnSerial
  :: Label
  -> Integer
  -> Transaction
  -> Transaction
addTxnSerial lbl i (Transaction (TopLine tl) bal) = Transaction tl' bal
  where
    tl' = TopLine $ M.insert lbl val tl
    val = PayDecimal . fromInteger $ i

addPstgSerial
  :: Label
  -> Integer
  -> PstgMeta
  -> PstgMeta
addPstgSerial lbl i (PstgMeta fields tri) = PstgMeta fields' tri
  where
    fields' = M.insert lbl (PayDecimal . fromInteger $ i) fields

assignTxnFwd
  :: Label
  -> Transaction
  -> State Integer Transaction
assignTxnFwd lbl txn = do
  this <- get
  modify succ
  return $ addTxnSerial lbl this txn

assignTxnRev
  :: Label
  -> Transaction
  -> State Integer Transaction
assignTxnRev lbl txn = do
  modify pred
  this <- get
  return $ addTxnSerial lbl this txn

assignPstgFwd
  :: Label
  -> PstgMeta
  -> State Integer PstgMeta
assignPstgFwd lbl pstg = do
  this <- get
  modify succ
  return $ addPstgSerial lbl this pstg

assignPstgRev
  :: Label
  -> PstgMeta
  -> State Integer PstgMeta
assignPstgRev lbl pstg = do
  modify pred
  this <- get
  return $ addPstgSerial lbl this pstg

