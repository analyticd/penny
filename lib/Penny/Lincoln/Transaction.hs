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
  Error ( UnbalancedError, CouldNotInferError),
  toUnverified,

  -- * Querying postings
  Inferred(Inferred, NotInferred),
  pPayee, pNumber, pFlag, pAccount, pTags,
  pEntry, pMemo, pInferred, pMeta, changePostingMeta,

  -- * Querying transactions
  TopLine,
  tDateTime, tFlag, tNumber, tPayee, tMemo, tMeta,
  unTransaction, postFam, changeTransactionMeta,

  -- * Adding serials
  addSerialsToList, addSerialsToEithers,

  -- * Box
  Box ( Box, boxMeta, boxPostFam ),

  -- * Changers

  -- | Functions allowing you to change aspects of an existing
  -- transaction, without having to destroy and completely rebuild the
  -- transaction. You cannot change the Entry or any of its
  -- components, as changing any of these would unbalance the
  -- Transaction.
  mapTopLine,
  cTxnDateTime,
  cTxnFlag,
  cTxnNumber,
  cTxnPayee,
  cTxnMemo,
  cTxnMeta,

  mapPostings,
  cPstgPayee,
  cPstgNumber,
  cPstgFlag,
  cPstgAccount,
  cPstgTags,
  cPstgMemo,
  cPstgMeta
  ) where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family ( children, orphans, adopt )
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Serial as Ser

import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Either as E
import qualified Data.Foldable as Fdbl
import Data.Maybe ( catMaybes )
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
          , pMeta     :: M.PostingMeta }
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
          , tMeta     :: M.TopLineMeta }
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
    fp (TopLine dt fl nu pa mo ma) = U.TopLine dt fl nu pa mo ma
    fc (Posting pa nu fl ac ta en mo ir me) =
      let maybeEn = case ir of
                      Inferred -> Nothing
                      NotInferred -> Just en
      in U.Posting pa nu fl ac ta maybeEn mo me

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
  . fmap (fmap Bal.entryToBalance . U.entry)

infer ::
  U.Posting
  -> Ex.ExceptionalT Error
  (St.State (Maybe B.Entry)) Posting
infer po =
  case U.entry po of
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

toPosting :: U.Posting
             -> B.Entry
             -> Inferred
             -> Posting
toPosting (U.Posting p n f a t _ m mt) e i =
  Posting p n f a t e m i mt

toTopLine :: U.TopLine -> TopLine
toTopLine (U.TopLine d f n p m mt) =
  TopLine d f n p m mt


-- | Change the metadata on a transaction.
changeTransactionMeta ::
  (M.TopLineMeta -> M.TopLineMeta)
  -- ^ Function that, when applied to the old top line meta, returns
  -- the new meta.

  -> Transaction
  -- ^ The old transaction with metadata

  -> Transaction
  -- ^ Transaction with new metadata

changeTransactionMeta fm (Transaction f) = Transaction f' where
  f' = F.Family tl c1 c2 cs
  (F.Family p c1 c2 cs) = f
  tl = p { tMeta = fm (tMeta tl) }

-- | Change the metadata on a posting.
changePostingMeta ::
  (M.PostingMeta -> M.PostingMeta)
  -> Transaction
  -> Transaction
changePostingMeta f (Transaction fam) =
  Transaction . F.mapChildren g $ fam
  where
    g p = p { pMeta = f (pMeta p) }

addSerials ::
  (Ser.Serial -> M.TopLineMeta -> M.TopLineMeta)
  -> (Ser.Serial -> M.PostingMeta -> M.PostingMeta)
  -> Ser.Serial
  -> Transaction
  -> St.State (Ser.NextFwd, Ser.NextBack) Transaction
addSerials ft fp s (Transaction fam) = do
  let topMapper pm = pm { tMeta = ft s (tMeta pm) }
      pstgMapper ser pstg = pstg { pMeta = fp ser (pMeta pstg) }
      fam' = F.mapParent topMapper fam
  fam'' <- Ser.serialChildrenInFamily pstgMapper fam'
  return $ Transaction fam''

addSerialsToList ::
  (Ser.Serial -> M.TopLineMeta -> M.TopLineMeta)
  -> (Ser.Serial -> M.PostingMeta -> M.PostingMeta)
  -> [Transaction]
  -> [Transaction]
addSerialsToList ft fp ls =
  let nPstgs = length . concatMap Fdbl.toList . map orphans
               . map unTransaction $ ls
      initState = Ser.initNexts nPstgs
      processor = addSerials ft fp
  in St.evalState (Ser.serialItemsM processor ls) initState


addSerialsToEithers ::
  (Ser.Serial -> M.TopLineMeta -> M.TopLineMeta)
  -> (Ser.Serial -> M.PostingMeta -> M.PostingMeta)
  -> [Either a Transaction]
  -> [Either a Transaction]
addSerialsToEithers ft fp ls =
  let txns = E.rights ls
      nPstgs = length . concatMap Fdbl.toList . map orphans
               . map unTransaction $ txns
      initState = Ser.initNexts nPstgs
      processA _ a = return a
      processTxn = addSerials ft fp
      k = Ser.serialEithers processA processTxn ls
  in St.evalState k initState

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

mapTopLine
  :: (TopLine -> TopLine)
  -> Transaction
  -> Transaction
mapTopLine f =
  Transaction
  . F.mapParent f
  . unTransaction

cTxnDateTime
  :: (Transaction -> B.DateTime)
  -> Transaction
  -> Transaction
cTxnDateTime f t = mapTopLine (\tl -> tl { tDateTime = f t }) t

cTxnFlag
  :: (Transaction -> Maybe B.Flag)
  -> Transaction
  -> Transaction
cTxnFlag f t = mapTopLine (\tl -> tl { tFlag = f t }) t

cTxnNumber
  :: (Transaction -> Maybe B.Number)
  -> Transaction
  -> Transaction
cTxnNumber f t = mapTopLine (\tl -> tl { tNumber = f t }) t

cTxnPayee
  :: (Transaction -> Maybe B.Payee)
  -> Transaction
  -> Transaction
cTxnPayee f t = mapTopLine (\tl -> tl { tPayee = f t }) t

cTxnMemo
  :: (Transaction -> Maybe B.Memo)
  -> Transaction
  -> Transaction
cTxnMemo f t = mapTopLine (\tl -> tl { tMemo = f t }) t

cTxnMeta
  :: (Transaction -> M.TopLineMeta)
  -> Transaction
  -> Transaction
cTxnMeta f t = mapTopLine (\tl -> tl { tMeta = f t }) t

mapPostings
  :: (Transaction -> Posting -> Posting)
  -> Transaction
  -> Transaction
mapPostings f t =
  Transaction
  . F.mapChildren (f t)
  . unTransaction
  $ t

cPstgPayee
  :: (Transaction -> Posting -> Maybe B.Payee)
  -> Transaction
  -> Transaction
cPstgPayee f = mapPostings (\t p -> p { pPayee = f t p })


cPstgNumber
  :: (Transaction -> Posting -> Maybe B.Number)
  -> Transaction
  -> Transaction
cPstgNumber f = mapPostings (\t p -> p { pNumber = f t p })

cPstgFlag
  :: (Transaction -> Posting -> Maybe B.Flag)
  -> Transaction
  -> Transaction
cPstgFlag f = mapPostings (\t p -> p { pFlag = f t p })

cPstgAccount
  :: (Transaction -> Posting -> B.Account)
  -> Transaction
  -> Transaction
cPstgAccount f = mapPostings (\t p -> p { pAccount = f t p })

cPstgTags
  :: (Transaction -> Posting -> B.Tags)
  -> Transaction
  -> Transaction
cPstgTags f = mapPostings (\t p -> p { pTags = f t p })

cPstgMemo
  :: (Transaction -> Posting -> Maybe B.Memo)
  -> Transaction
  -> Transaction
cPstgMemo f = mapPostings (\t p -> p { pMemo = f t p })

cPstgMeta
  :: (Transaction -> Posting -> M.PostingMeta)
  -> Transaction
  -> Transaction
cPstgMeta f = mapPostings (\t p -> p { pMeta = f t p })







