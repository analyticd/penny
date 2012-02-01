module Penny.Lincoln.Transaction (
  Posting,
  TopLine,
  Transaction,
  unTransaction,
  transaction,
  Error ( UnbalancedError, TooManyInferError,
          CouldNotInferError),
  postingFamily) where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family ( children, orphans, adopt )
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Balance as B

import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import Data.Monoid ( mconcat )
import Data.Maybe ( catMaybes )
import qualified Data.Traversable as Tr
import qualified Control.Monad.Trans.State.Lazy as St
import Control.Monad.Trans.Class ( lift )

data Posting =
  Posting { payee :: Maybe B.Payee
          , number :: Maybe B.Number
          , flag :: Maybe B.Flag
          , account :: B.Account
          , tags :: B.Tags
          , entry :: E.Entry
          , memo :: Maybe B.Memo }
  deriving Show

data TopLine =
  TopLine { dateTime :: B.DateTime
          , flag :: Maybe B.Flag
          , number :: Maybe B.Number
          , payee :: Maybe B.Payee
          , memo :: Maybe B.Memo }
  deriving Show

-- | All the Postings in a Transaction:
--
-- * Must produce a Total whose debits and credits are equal.
newtype Transaction =
  Transaction { unTransaction :: F.Family TopLine Posting }
  deriving Show
  
data Error = UnbalancedError
           | TooManyInferError
           | CouldNotInferError

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postingFamily :: Transaction -> Siblings (Child P.TopLine Posting)
postingFamily (Transaction ps) = children ps

-- | Makes transactions.
transaction ::
  Family U.TopLine U.Posting
  -> Exceptional Error Transaction
transaction f@(Family p _ _ _) = do
  let os = orphans f
      t = totalAll os
      p' = toTopLine p
  a2 <- inferAll os t
  return $ Transaction (adopt p' a2)

totalAll :: S.Siblings U.Posting
         -> T.Total
totalAll =
  mconcat
  . catMaybes
  . F.toList
  . fmap (fmap T.entryToTotal . U.entry)

infer ::
  UPosting.Posting
  -> Ex.ExceptionalT Error
  (St.State (Maybe E.Entry)) Posting
infer po =
  case U.entry po of
    Nothing -> do
      st <- lift St.get
      case st of
        Nothing -> Ex.throwT CouldNotInferError
        (Just e) -> do
          lift $ St.put Nothing
          return $ toPosting po e
    (Just e) -> return $ toPosting po e
          
runInfer ::
  Maybe E.Entry
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
  S.SiblingsPosting
  -> T.Total
  -> Exceptional Error (S.Siblings Posting)
inferAll pos t = do
  en <- case T.isBalanced t of
    T.Balanced -> return Nothing
    (T.Inferable e) -> return $ Just e
    T.NotInferable -> throw UnbalancedError
  runInfer en pos

toPosting :: U.Posting
             -> B.Entry
             -> Posting
toPosting (U.Posting p n f a t _ m) e = Posting p n f a t e m

toTopLine :: U.TopLine -> TopLine
toTopLine (U.TopLine d f n p m) = TopLine d f n p m


