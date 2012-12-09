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
  toUnverified,

  -- * Querying postings
  Inferred(Inferred, NotInferred),
  pPayee, pNumber, pFlag, pAccount, pTags,
  pEntry, pMemo, pInferred, pPostingLine,
  pGlobalPosting, pFilePosting,

  -- * Querying transactions
  TopLine,
  tDateTime, tFlag, tNumber, tPayee, tMemo, tTopLineLine,
  tTopMemoLine, tFilename, tGlobalTransaction, tFileTransaction,
  unTransaction, postFam,

  -- * Box
  Box ( Box, boxMeta, boxPostFam ),

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

import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import Data.Maybe ( catMaybes, fromMaybe )
import qualified Data.Traversable as Tr
import qualified Control.Monad.Trans.State.Lazy as St
import Control.Monad.Trans.Class ( lift )

-- | Indicates whether the entry for this posting was inferred. That
-- is, if the user did not supply an entry for this posting, then it
-- was inferred.
data Inferred = Inferred | NotInferred deriving (Eq, Show)

-- | Each Transaction consists of at least two Postings.
data Posting =
  Posting { pPayee    :: Maybe B.Payee
          , pNumber   :: Maybe B.Number
          , pFlag     :: Maybe B.Flag
          , pAccount  :: B.Account
          , pTags     :: B.Tags
          , pEntry    :: B.Entry
          , pMemo     :: Maybe B.Memo
          , pInferred :: Inferred
          , pPostingLine :: Maybe B.PostingLine
          , pGlobalPosting :: Maybe B.GlobalPosting
          , pFilePosting :: Maybe B.FilePosting
          }
  deriving (Eq, Show)

-- | The TopLine holds information that applies to all the postings in
-- a transaction (so named because in a ledger file, this information
-- appears on the top line.)
data TopLine =
  TopLine { tDateTime :: B.DateTime
          , tFlag     :: Maybe B.Flag
          , tNumber   :: Maybe B.Number
          , tPayee    :: Maybe B.Payee
          , tMemo     :: Maybe B.Memo
          , tTopLineLine :: Maybe B.TopLineLine
          , tTopMemoLine :: Maybe B.TopMemoLine
          , tFilename :: Maybe B.Filename
          , tGlobalTransaction :: Maybe B.GlobalTransaction
          , tFileTransaction :: Maybe B.FileTransaction }
  deriving (Eq, Show)

-- | All the Postings in a Transaction must produce a Total whose
-- debits and credits are equal. That is, the Transaction must be
-- balanced. No Transactions are created that are not balanced.
newtype Transaction =
  Transaction { unTransaction :: F.Family TopLine Posting }
  deriving (Eq, Show)

-- | Errors that can arise when making a Transaction.
data Error = UnbalancedError
           | CouldNotInferError
           deriving (Eq, Show)

newtype PostFam = PostFam { unPostFam :: C.Child TopLine Posting }
                  deriving Show

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postFam :: Transaction -> [PostFam]
postFam (Transaction ps) = map PostFam . Fdbl.toList . children $ ps

{- BNF-like grammar for the various sorts of allowed postings.

postingGroup ::= (inferGroup balancedGroup*) | balancedGroup+
inferGroup ::= "at least 1 posting. All postings have same account and
                commodity. The balance is inferable."
balancedGroup ::= "at least 2 postings. All postings have the same
                   account and commodity. The balance is balanced."

-}

-- | Deconstruct a Transaction to a family of unverified data.
toUnverified :: Transaction -> F.Family U.TopLine U.Posting
toUnverified = F.mapParent fp . F.mapChildren fc . unTransaction
  where
    fp tl = toUTopLine tl
    fc p = toUPosting p

-- | Makes transactions.
transaction ::
  F.Family U.TopLine U.Posting
  -> Exceptional Error Transaction
transaction f@(F.Family p _ _ _) = do
  let os = orphans f
      t = totalAll os
      p' = toTopLine p
  a2 <- inferAll os t
  return $ Transaction (adopt p' a2)

totalAll :: S.Siblings U.Posting
         -> Bal.Balance
totalAll =
  Fdbl.foldr1 Bal.addBalances
  . catMaybes
  . Fdbl.toList
  . fmap (fmap Bal.entryToBalance . U.pEntry)

infer ::
  U.Posting
  -> Ex.ExceptionalT Error
  (St.State (Maybe B.Entry)) Posting
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
  -> S.Siblings U.Posting
  -> Exceptional Error (S.Siblings Posting)
runInfer me pos = do
  let (res, finalSt) = St.runState ext me
      ext = Ex.runExceptionalT (Tr.mapM infer pos)
  case finalSt of
    (Just _) -> throw UnbalancedError
    Nothing -> case res of
      (Exception e) -> throw e
      (Success g) -> return g

inferAll ::
  S.Siblings U.Posting
  -> Bal.Balance
  -> Exceptional Error (S.Siblings Posting)
inferAll pos t = do
  en <- case Bal.isBalanced t of
    Bal.Balanced -> return Nothing
    (Bal.Inferable e) -> return $ Just e
    Bal.NotInferable -> throw UnbalancedError
  runInfer en pos

toUPosting :: Posting -> U.Posting
toUPosting p = U.Posting
  { U.pPayee = pPayee p
  , U.pNumber = pNumber p
  , U.pFlag = pFlag p
  , U.pAccount = pAccount p
  , U.pTags = pTags p
  , U.pEntry = case pInferred p of
      Inferred -> Nothing
      NotInferred -> Just (pEntry p)
  , U.pMemo = pMemo p
  , U.pPostingLine = pPostingLine p
  , U.pGlobalPosting = pGlobalPosting p
  , U.pFilePosting = pFilePosting p
  }


toPosting :: U.Posting
             -> B.Entry
             -> Inferred
             -> Posting
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
  , pGlobalPosting = U.pGlobalPosting u
  , pFilePosting = U.pFilePosting u
  }


toUTopLine :: TopLine -> U.TopLine
toUTopLine t = U.TopLine
  { U.tDateTime = tDateTime t
  , U.tFlag     = tFlag t
  , U.tNumber   = tNumber t
  , U.tPayee    = tPayee t
  , U.tMemo     = tMemo t
  , U.tTopLineLine = tTopLineLine t
  , U.tTopMemoLine = tTopMemoLine t
  , U.tFilename = tFilename t
  , U.tGlobalTransaction = tGlobalTransaction t
  , U.tFileTransaction = tFileTransaction t
  }

toTopLine :: U.TopLine -> TopLine
toTopLine t = TopLine
  { tDateTime = U.tDateTime t
  , tFlag     = U.tFlag t
  , tNumber   = U.tNumber t
  , tPayee    = U.tPayee t
  , tMemo     = U.tMemo t
  , tTopLineLine = U.tTopLineLine t
  , tTopMemoLine = U.tTopMemoLine t
  , tFilename = U.tFilename t
  , tGlobalTransaction = U.tGlobalTransaction t
  , tFileTransaction = U.tFileTransaction t
  }

fromRPosting :: U.RPosting -> B.Entry -> Inferred -> Posting
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
  , pGlobalPosting = U.rGlobalPosting u
  , pFilePosting = U.rFilePosting u
  }

fromIPosting :: U.IPosting -> B.Entry -> Inferred -> Posting
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
  , pGlobalPosting = U.iGlobalPosting u
  , pFilePosting = U.iFilePosting u
  }

data RTransaction = RTransaction
  { rtCommodity :: B.Commodity
    -- ^ All postings will have this same commodity

  , rtSide :: Maybe B.Side
  -- ^ All commodities will be on this side of the amount

  , rtSpaceBetween :: Maybe B.SpaceBetween
  -- ^ All amounts will have this SpaceBetween

  , rtDrCr :: B.DrCr
  -- ^ All postings except the inferred one will have this DrCr

  , rtTopLine :: U.TopLine

  , rtPosting :: U.RPosting
  -- ^ You must have at least one posting whose quantity you specify

  , rtMorePostings :: [U.RPosting]
  -- ^ Optionally you can have additional restricted postings.

  , rtIPosting :: U.IPosting
  -- ^ And at least one posting whose quantity and DrCr will be inferred

  } deriving Show

-- | Creates a @restricted transaction@; that is, one in which all the
-- entries will have the same commodity, and in which all but one of
-- the postings will all be debits or credits. The last posting will
-- have no quantity specified at all and will be inferred. Creating
-- these transactions never fails, in contrast to the transactions
-- created by 'transaction', which can fail at runtime.
rTransaction :: RTransaction -> Transaction
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

-- | A box stores a family of transaction data along with
-- metadata. The transaction is stored in child form, indicating a
-- particular posting of interest. The metadata is in addition to the
-- metadata associated with the TopLine and with each posting.
data Box m =
  Box { boxMeta :: m
      , boxPostFam :: PostFam }
  deriving Show

instance Functor Box where
  fmap f (Box m pf) = Box (f m) pf

-------------------------------------------------------------
-- Changers
-------------------------------------------------------------

-- | Each field in the record is a Maybe. If Nothing, make no change
-- to this part of the TopLine.
data TopLineChangeData = TopLineChangeData
  { tcDateTime :: Maybe B.DateTime
  , tcFlag :: Maybe (Maybe B.Flag)
  , tcNumber :: Maybe (Maybe B.Number)
  , tcPayee :: Maybe (Maybe B.Payee)
  , tcMemo :: Maybe (Maybe B.Memo)
  , tcTopLineLine :: Maybe (Maybe B.TopLineLine)
  , tcTopMemoLine :: Maybe (Maybe B.TopMemoLine)
  , tcFilename :: Maybe (Maybe B.Filename)
  , tcGlobalTransaction :: Maybe (Maybe B.GlobalTransaction)
  , tcFileTransaction :: Maybe (Maybe B.FileTransaction)
  } deriving Show

emptyTopLineChangeData :: TopLineChangeData
emptyTopLineChangeData = TopLineChangeData
  { tcDateTime = Nothing
  , tcFlag = Nothing
  , tcNumber = Nothing
  , tcPayee = Nothing
  , tcMemo = Nothing
  , tcTopLineLine = Nothing
  , tcTopMemoLine = Nothing
  , tcFilename = Nothing
  , tcGlobalTransaction = Nothing
  , tcFileTransaction = Nothing
  }


applyTopLineChange :: TopLineChangeData -> TopLine -> TopLine
applyTopLineChange c t = TopLine
  { tDateTime = fromMaybe (tDateTime t) (tcDateTime c)
  , tFlag = fromMaybe (tFlag t) (tcFlag c)
  , tNumber = fromMaybe (tNumber t) (tcNumber c)
  , tPayee = fromMaybe (tPayee t) (tcPayee c)
  , tMemo = fromMaybe (tMemo t) (tcMemo c)
  , tTopLineLine = fromMaybe (tTopLineLine t) (tcTopLineLine c)
  , tTopMemoLine = fromMaybe (tTopMemoLine t) (tcTopMemoLine c)
  , tFilename = fromMaybe (tFilename t) (tcFilename c)
  , tGlobalTransaction = fromMaybe (tGlobalTransaction t)
                         (tcGlobalTransaction c)
  , tFileTransaction = fromMaybe (tFileTransaction t)
                       (tcFileTransaction c)
  }

data PostingChangeData = PostingChangeData
  { pcPayee :: Maybe (Maybe B.Payee)
  , pcNumber :: Maybe (Maybe B.Number)
  , pcFlag :: Maybe (Maybe B.Flag)
  , pcAccount :: Maybe B.Account
  , pcTags :: Maybe B.Tags
  , pcMemo :: Maybe (Maybe B.Memo)
  , pcSide :: Maybe (Maybe B.Side)
  , pcSpaceBetween :: Maybe (Maybe B.SpaceBetween)
  , pcPostingLine :: Maybe (Maybe B.PostingLine)
  , pcGlobalPosting :: Maybe (Maybe B.GlobalPosting)
  , pcFilePosting :: Maybe (Maybe B.FilePosting)
  } deriving Show

emptyPostingChangeData :: PostingChangeData
emptyPostingChangeData = PostingChangeData
  { pcPayee = Nothing
  , pcNumber = Nothing
  , pcFlag = Nothing
  , pcAccount = Nothing
  , pcTags = Nothing
  , pcMemo = Nothing
  , pcSide = Nothing
  , pcSpaceBetween = Nothing
  , pcPostingLine = Nothing
  , pcGlobalPosting = Nothing
  , pcFilePosting = Nothing
  }


applyPostingChange :: PostingChangeData -> Posting -> Posting
applyPostingChange c p = Posting
  { pPayee = fromMaybe (pPayee p) (pcPayee c)
  , pNumber = fromMaybe (pNumber p) (pcNumber c)
  , pFlag = fromMaybe (pFlag p) (pcFlag c)
  , pAccount = fromMaybe (pAccount p) (pcAccount c)
  , pTags = fromMaybe (pTags p) (pcTags c)
  , pEntry = en
  , pMemo = fromMaybe (pMemo p) (pcMemo c)
  , pInferred = pInferred p
  , pPostingLine = fromMaybe (pPostingLine p) (pcPostingLine c)
  , pGlobalPosting = fromMaybe (pGlobalPosting p) (pcGlobalPosting c)
  , pFilePosting = fromMaybe (pFilePosting p) (pcFilePosting c)
  }
  where
    enOld = pEntry p
    amOld = B.amount enOld
    en = B.Entry (B.drCr enOld) am
    am = B.Amount (B.qty amOld) (B.commodity amOld) sd sb
    sd = fromMaybe (B.side amOld) (pcSide c)
    sb = fromMaybe (B.spaceBetween amOld) (pcSpaceBetween c)

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
  :: F.Family TopLineChangeData PostingChangeData
  -> Transaction
  -> Transaction
changeTransaction c (Transaction t) =
  let F.Family ctl cp1 cp2 cps = c
      F.Family tl p1 p2 ps = t
      tl' = applyTopLineChange ctl tl
      p1' = applyPostingChange cp1 p1
      p2' = applyPostingChange cp2 p2
      ps' = zipWith applyPostingChange
            (cps ++ repeat emptyPostingChangeData) ps
  in Transaction (F.Family tl' p1' p2' ps')
