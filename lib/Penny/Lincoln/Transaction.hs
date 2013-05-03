{-# LANGUAGE DeriveGeneric #-}

-- | Transactions, the heart of Penny. The Transaction data type is
-- abstract, so that only this module can create Transactions. This
-- provides assurance that if a Transaction exists, it is a valid,
-- balanced Transaction. In addition, the Posting data type is
-- abstract as well, so you know that if you have a Posting, it was
-- created as part of a balanced Transaction.
--
-- Functions prefixed with a @p@ query a particular posting for its
-- properties. Functions prefixed with a @t@ query transactions. Every
-- transaction has a single DateTime, and all the postings have this
-- same DateTime, so there is no function to query a posting's
-- DateTime. Just query the parent transaction. For other things such
-- as Number and Flag, the transaction might have data and the posting
-- might have data as well, so functions are provided to query both.
--
-- Often you will want to query a single posting and have a function
-- that gives you, for example, the posting's flag if it has one, or
-- the transaction's flag if it has one, or Nothing if neither the
-- posting nor the transaction has a flag. The functions in
-- "Penny.Lincoln.Queries" do that.
module Penny.Lincoln.Transaction (

  -- * Postings and transactions
  Posting,
  Transaction,
  PostFam,
  unPostFam,

  -- * Making and deconstructing transactions
  transaction,
  RTransaction(..),
  rTransaction,
  Error ( UnbalancedError, CouldNotInferError),

  -- * Querying postings
  Inferred(Inferred, NotInferred),
  pPayee, pNumber, pFlag, pAccount, pTags,
  pEntry, pMemo, pInferred, pPostingLine,
  pFilePosting, pMeta,

  -- * Querying transactions
  TopLine,
  tDateTime, tFlag, tNumber, tPayee, tMemo, tTopLineLine,
  tTopMemoLine, tFileTransaction, tMeta,
  unTransaction, postFam,

  -- * Changers

  -- | Functions allowing you to change aspects of an existing
  -- transaction, without having to destroy and completely rebuild the
  -- transaction. You cannot change the Entry or any of its
  -- components, as changing any of these would unbalance the
  -- Transaction.
  TopLineChangeData(..),
  emptyTopLineChangeData,
  PostingChangeData(..),
  emptyPostingChangeData,
  changeTransaction
  ) where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family ( children, orphans, adopt )
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Balance as Bal

import qualified Data.Binary as B
import GHC.Generics (Generic)
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import Data.Maybe ( catMaybes )
import qualified Data.Traversable as Tr
import qualified Control.Monad.Trans.State.Lazy as St
import Control.Monad.Trans.Class ( lift )

-- | Indicates whether the entry for this posting was inferred. That
-- is, if the user did not supply an entry for this posting, then it
-- was inferred.
data Inferred = Inferred | NotInferred deriving (Eq, Show, Generic)

instance B.Binary Inferred

-- | Each Transaction consists of at least two Postings.
data Posting pm =
  Posting { pPayee    :: Maybe B.Payee
          , pNumber   :: Maybe B.Number
          , pFlag     :: Maybe B.Flag
          , pAccount  :: B.Account
          , pTags     :: B.Tags
          , pEntry    :: B.Entry
          , pMemo     :: Maybe B.Memo
          , pInferred :: Inferred
          , pPostingLine :: Maybe B.PostingLine
          , pFilePosting :: Maybe B.FilePosting
          , pMeta :: pm
          }
  deriving (Eq, Show, Generic)

instance B.Binary pm => B.Binary (Posting pm)

-- | The TopLine holds information that applies to all the postings in
-- a transaction (so named because in a ledger file, this information
-- appears on the top line.)
data TopLine tm =
  TopLine { tDateTime :: B.DateTime
          , tFlag     :: Maybe B.Flag
          , tNumber   :: Maybe B.Number
          , tPayee    :: Maybe B.Payee
          , tMemo     :: Maybe B.Memo
          , tTopLineLine :: Maybe B.TopLineLine
          , tTopMemoLine :: Maybe B.TopMemoLine
          , tFileTransaction :: Maybe B.FileTransaction
          , tMeta :: tm }
  deriving (Eq, Show, Generic)

instance B.Binary tm => B.Binary (TopLine tm)

-- | All the Postings in a Transaction must produce a Total whose
-- debits and credits are equal. That is, the Transaction must be
-- balanced. No Transactions are created that are not balanced.
newtype Transaction tm pm =
  Transaction { unTransaction :: F.Family (TopLine tm) (Posting pm) }
  deriving (Eq, Show, Generic)

instance (B.Binary tm, B.Binary pm) => B.Binary (Transaction tm pm)

-- | Errors that can arise when making a Transaction.
data Error = UnbalancedError
           | CouldNotInferError
           deriving (Eq, Show)

newtype PostFam tm pm = PostFam
  { unPostFam :: C.Child (TopLine tm) (Posting pm) }
  deriving (Show, Generic)

instance (B.Binary tm, B.Binary pm) => B.Binary (PostFam tm pm)

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postFam :: Transaction tm pm -> [PostFam tm pm]
postFam (Transaction ps) = map PostFam . Fdbl.toList . children $ ps

{- BNF-like grammar for the various sorts of allowed postings.

postingGroup ::= (inferGroup balancedGroup*) | balancedGroup+
inferGroup ::= "at least 1 posting. All postings have same account and
                commodity. The balance is inferable."
balancedGroup ::= "at least 2 postings. All postings have the same
                   account and commodity. The balance is balanced."

-}

-- | Makes transactions.
transaction ::
  F.Family (U.TopLine tm) (U.Posting pm)
  -> Exceptional Error (Transaction tm pm)
transaction f@(F.Family p _ _ _) = do
  let os = orphans f
      t = totalAll os
      p' = toTopLine p
  a2 <- inferAll os t
  return $ Transaction (adopt p' a2)

totalAll :: S.Siblings (U.Posting pm)
         -> Bal.Balance
totalAll =
  Fdbl.foldr1 Bal.addBalances
  . catMaybes
  . Fdbl.toList
  . fmap (fmap Bal.entryToBalance . U.pEntry)

infer ::
  U.Posting pm
  -> Ex.ExceptionalT Error
  (St.State (Maybe B.Entry)) (Posting pm)
infer po =
  case U.pEntry po of
    Nothing -> do
      st <- lift St.get
      case st of
        Nothing -> Ex.throwT CouldNotInferError
        (Just e) -> do
          lift $ St.put Nothing
          return $ toPosting po e Inferred
    (Just e) -> return $ toPosting po e NotInferred

runInfer ::
  Maybe B.Entry
  -> S.Siblings (U.Posting pm)
  -> Exceptional Error (S.Siblings (Posting pm))
runInfer me pos = do
  let (res, finalSt) = St.runState ext me
      ext = Ex.runExceptionalT (Tr.mapM infer pos)
  case finalSt of
    (Just _) -> throw UnbalancedError
    Nothing -> case res of
      (Exception e) -> throw e
      (Success g) -> return g

inferAll ::
  S.Siblings (U.Posting pm)
  -> Bal.Balance
  -> Exceptional Error (S.Siblings (Posting pm))
inferAll pos t = do
  en <- case Bal.isBalanced t of
    Bal.Balanced -> return Nothing
    (Bal.Inferable e) -> return $ Just e
    Bal.NotInferable -> throw UnbalancedError
  runInfer en pos


toPosting :: U.Posting pm
             -> B.Entry
             -> Inferred
             -> Posting pm
toPosting u e i =
  Posting
  { pPayee    = U.pPayee u
  , pNumber   = U.pNumber u
  , pFlag     = U.pFlag u
  , pAccount  = U.pAccount u
  , pTags     = U.pTags u
  , pEntry    = e
  , pMemo     = U.pMemo u
  , pInferred = i
  , pPostingLine = U.pPostingLine u
  , pFilePosting = U.pFilePosting u
  , pMeta = U.pMeta u
  }


toTopLine :: U.TopLine tm -> TopLine tm
toTopLine t = TopLine
  { tDateTime = U.tDateTime t
  , tFlag     = U.tFlag t
  , tNumber   = U.tNumber t
  , tPayee    = U.tPayee t
  , tMemo     = U.tMemo t
  , tTopLineLine = U.tTopLineLine t
  , tTopMemoLine = U.tTopMemoLine t
  , tFileTransaction = U.tFileTransaction t
  , tMeta = U.tMeta t
  }

fromRPosting :: U.RPosting pm -> B.Entry -> Inferred -> Posting pm
fromRPosting u e i = Posting
  { pPayee    = U.rPayee u
  , pNumber   = U.rNumber u
  , pFlag     = U.rFlag u
  , pAccount  = U.rAccount u
  , pTags     = U.rTags u
  , pEntry    = e
  , pMemo     = U.rMemo u
  , pInferred = i
  , pPostingLine = U.rPostingLine u
  , pFilePosting = U.rFilePosting u
  , pMeta = U.rMeta u
  }

fromIPosting :: U.IPosting pm -> B.Entry -> Inferred -> Posting pm
fromIPosting u e i = Posting
  { pPayee    = U.iPayee u
  , pNumber   = U.iNumber u
  , pFlag     = U.iFlag u
  , pAccount  = U.iAccount u
  , pTags     = U.iTags u
  , pEntry    = e
  , pMemo     = U.iMemo u
  , pInferred = i
  , pPostingLine = U.iPostingLine u
  , pFilePosting = U.iFilePosting u
  , pMeta = U.iMeta u
  }

data RTransaction tm pm = RTransaction
  { rtCommodity :: B.Commodity
    -- ^ All postings will have this same commodity

  , rtSide :: Maybe B.Side
  -- ^ All commodities will be on this side of the amount

  , rtSpaceBetween :: Maybe B.SpaceBetween
  -- ^ All amounts will have this SpaceBetween

  , rtDrCr :: B.DrCr
  -- ^ All postings except the inferred one will have this DrCr

  , rtTopLine :: U.TopLine tm

  , rtPosting :: U.RPosting pm
  -- ^ You must have at least one posting whose quantity you specify

  , rtMorePostings :: [U.RPosting pm]
  -- ^ Optionally you can have additional restricted postings.

  , rtIPosting :: U.IPosting pm
  -- ^ And at least one posting whose quantity and DrCr will be inferred

  } deriving Show

-- | Creates a @restricted transaction@; that is, one in which all the
-- entries will have the same commodity, and in which all but one of
-- the postings will all be debits or credits. The last posting will
-- have no quantity specified at all and will be inferred. Creating
-- these transactions never fails, in contrast to the transactions
-- created by 'transaction', which can fail at runtime.
rTransaction :: RTransaction tm pm -> Transaction tm pm
rTransaction rt = Transaction (F.Family tl p1 p2 ps)
  where
    tl = toTopLine (rtTopLine rt)
    tot = foldl1 B.add $ (U.rQty . rtPosting $ rt)
                         : map U.rQty (rtMorePostings rt)
    sd = rtSide rt
    sb = rtSpaceBetween rt
    inf = fromIPosting (rtIPosting rt)
          ( B.Entry (B.opposite (rtDrCr rt))
            (B.Amount tot (rtCommodity rt) sd sb))
          Inferred
    toPstg p = fromRPosting p
               (B.Entry (rtDrCr rt)
               (B.Amount (U.rQty p) (rtCommodity rt) sd sb)) NotInferred
    p1 = toPstg . rtPosting $ rt
    (p2, ps) = case rtMorePostings rt of
      [] -> (inf, [])
      x:xs -> (toPstg x, (map toPstg xs) ++ [inf])

-------------------------------------------------------------
-- Changers
-------------------------------------------------------------

-- | Each field in the record is a Maybe. If Nothing, make no change
-- to this part of the TopLine.
data TopLineChangeData om nm = TopLineChangeData
  { tcDateTime :: B.DateTime -> B.DateTime
  , tcFlag :: Maybe B.Flag -> Maybe B.Flag
  , tcNumber :: Maybe B.Number -> Maybe B.Number
  , tcPayee :: Maybe B.Payee -> Maybe B.Payee
  , tcMemo :: Maybe B.Memo -> Maybe B.Memo
  , tcTopLineLine :: Maybe B.TopLineLine -> Maybe B.TopLineLine
  , tcTopMemoLine :: Maybe B.TopMemoLine -> Maybe B.TopMemoLine
  , tcFileTransaction :: Maybe B.FileTransaction -> Maybe B.FileTransaction
  , tcMeta :: om -> nm
  }

emptyTopLineChangeData :: (om -> tm) -> TopLineChangeData om tm
emptyTopLineChangeData f = TopLineChangeData
  { tcDateTime = id
  , tcFlag = id
  , tcNumber = id
  , tcPayee = id
  , tcMemo = id
  , tcTopLineLine = id
  , tcTopMemoLine = id
  , tcFileTransaction = id
  , tcMeta = f
  }


applyTopLineChange :: TopLineChangeData om nm -> TopLine om -> TopLine nm
applyTopLineChange c t = TopLine
  { tDateTime = (tcDateTime c) (tDateTime t)
  , tFlag = (tcFlag c) (tFlag t)
  , tNumber = (tcNumber c) (tNumber t)
  , tPayee = (tcPayee c) (tPayee t)
  , tMemo = (tcMemo c) (tMemo t)
  , tTopLineLine = (tcTopLineLine c) (tTopLineLine t)
  , tTopMemoLine = (tcTopMemoLine c) (tTopMemoLine t)
  , tFileTransaction = (tcFileTransaction c) (tFileTransaction t)
  , tMeta = (tcMeta c) (tMeta t)
  }

data PostingChangeData om nm = PostingChangeData
  { pcPayee :: Maybe B.Payee -> Maybe B.Payee
  , pcNumber :: Maybe B.Number -> Maybe B.Number
  , pcFlag :: Maybe B.Flag -> Maybe B.Flag
  , pcAccount :: B.Account -> B.Account
  , pcTags :: B.Tags -> B.Tags
  , pcMemo :: Maybe B.Memo ->  Maybe B.Memo
  , pcSide :: Maybe B.Side -> Maybe B.Side
  , pcSpaceBetween :: Maybe B.SpaceBetween -> Maybe B.SpaceBetween
  , pcPostingLine :: Maybe B.PostingLine -> Maybe B.PostingLine
  , pcFilePosting :: Maybe B.FilePosting -> Maybe B.FilePosting
  , pcMeta :: om -> nm
  }

emptyPostingChangeData :: (om -> nm) -> PostingChangeData om nm
emptyPostingChangeData f = PostingChangeData
  { pcPayee = id
  , pcNumber = id
  , pcFlag = id
  , pcAccount = id
  , pcTags = id
  , pcMemo = id
  , pcSide = id
  , pcSpaceBetween = id
  , pcPostingLine = id
  , pcFilePosting = id
  , pcMeta = f
  }


applyPostingChange :: PostingChangeData om nm -> Posting om -> Posting nm
applyPostingChange c p = Posting
  { pPayee = (pcPayee c) (pPayee p)
  , pNumber = (pcNumber c) (pNumber p)
  , pFlag = (pcFlag c) (pFlag p)
  , pAccount = (pcAccount c) (pAccount p)
  , pTags = (pcTags c) (pTags p)
  , pEntry = en
  , pMemo =  (pcMemo c) (pMemo p)
  , pInferred = pInferred p
  , pPostingLine = (pcPostingLine c) (pPostingLine p)
  , pFilePosting = (pcFilePosting c) (pFilePosting p)
  , pMeta = (pcMeta c) (pMeta p)
  }
  where
    enOld = pEntry p
    amOld = B.amount enOld
    en = B.Entry (B.drCr enOld) am
    am = B.Amount (B.qty amOld) (B.commodity amOld) sd sb
    sd = (pcSide c) (B.side amOld)
    sb = (pcSpaceBetween c) (B.spaceBetween amOld)

-- | Allows you to change the parts of a transaction that can be
-- chanaged without unbalancing the transaction. You cannot change the
-- DrCr, Qty, or Commodity, as changing these might unbalance the
-- transaction. If there are elements you do not want to change at
-- all, use an 'emptyTopLineChangeData' or an 'emptyPostingChangeData'
-- in the appropriate part of the Family that you pass in. If the
-- Family of change data has more children than the transaction, these
-- extra children are ignored. If the Family in the Transaction has
-- more children than the Family of change data, the extra postings
-- are unchanged. That is, 'changeTransaction' will never delete
-- postings.
changeTransaction
  :: (opm -> npm)
  -- ^ Apply this function to change the posting metadata of left-side
  -- postings that have no change data

  -> F.Family (TopLineChangeData otm ntm) (PostingChangeData opm npm)


  -> Transaction otm opm
  -> Transaction ntm npm
changeTransaction f c (Transaction t) =
  let F.Family ctl cp1 cp2 cps = c
      F.Family tl p1 p2 ps = t
      tl' = applyTopLineChange ctl tl
      p1' = applyPostingChange cp1 p1
      p2' = applyPostingChange cp2 p2
      ps' = zipWith applyPostingChange
            (cps ++ repeat (emptyPostingChangeData f)) ps
  in Transaction (F.Family tl' p1' p2' ps')

