module Penny.Posting (
  Posting(..),
  Transaction,
  unTransaction,
  transaction,
  Error ( UnbalancedError, TooManyInferError,
          CouldNotInferError),
  postingFamily) where

import qualified Penny.Bits as B
import qualified Penny.Bits.Entry as E
import qualified Penny.Total as T
import Penny.Family.Family ( Family ( Family ))
import Penny.Family.Child ( Child )
import Penny.Family ( children, orphans, adopt )

import qualified Penny.Posting.Unverified.TopLine as UTopLine
import qualified Penny.Posting.Unverified.Posting as UPosting

import qualified Penny.Posting.TopLine as P
import Penny.Groups.AtLeast2 ( AtLeast2 )
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
          , entry :: E.Entry
          , tags :: B.Tags
          , memo :: Maybe B.Memo }
  deriving Show

-- | All the Postings in a Transaction:
--
-- * Must produce a Total whose debits and credits are equal.
newtype Transaction =
  Transaction { unTransaction :: Family P.TopLine Posting }
  deriving Show
  
data Error = UnbalancedError
           | TooManyInferError
           | CouldNotInferError

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postingFamily :: Transaction -> AtLeast2 (Child P.TopLine Posting)
postingFamily (Transaction ps) = children ps

-- | Makes transactions.
transaction ::
  Family UTopLine.TopLine UPosting.Posting
  -> Exceptional Error Transaction
transaction f@(Family p _ _ _) = do
  let os = orphans f
      t = totalAll os
      p' = toTopLine p
  a2 <- inferAll os t
  return $ Transaction (adopt p' a2)

totalAll :: AtLeast2 UPosting.Posting
         -> T.Total
totalAll =
  mconcat
  . catMaybes
  . F.toList
  . fmap (fmap T.entryToTotal . UPosting.entry)

infer ::
  UPosting.Posting
  -> Ex.ExceptionalT Error
  (St.State (Maybe E.Entry)) Posting
infer po =
  case UPosting.entry po of
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
  -> AtLeast2 UPosting.Posting
  -> Exceptional Error (AtLeast2 Posting)
runInfer me pos = do
  let (res, finalSt) = St.runState ext me
      ext = Ex.runExceptionalT (Tr.mapM infer pos)
  case finalSt of
    (Just _) -> throw UnbalancedError
    Nothing -> case res of 
      (Exception e) -> throw e
      (Success g) -> return g

inferAll ::
  AtLeast2 UPosting.Posting
  -> T.Total
  -> Exceptional Error (AtLeast2 Posting)
inferAll pos t = do
  en <- case T.isBalanced t of
    T.Balanced -> return Nothing
    (T.Inferable e) -> return $ Just e
    T.NotInferable -> throw UnbalancedError
  runInfer en pos

toPosting :: UPosting.Posting
             -> E.Entry
             -> Posting
toPosting po e = Posting { payee = UPosting.payee po
                         , number = UPosting.number po
                         , flag = UPosting.flag po
                         , account = UPosting.account po
                         , entry = e
                         , tags = UPosting.tags po
                         , memo = UPosting.memo po }

toTopLine :: UTopLine.TopLine -> P.TopLine
toTopLine pa = P.TopLine { P.dateTime = UTopLine.dateTime pa
                       , P.flag = UTopLine.flag pa
                       , P.number = UTopLine.number pa
                       , P.payee = UTopLine.payee pa
                       , P.memo = UTopLine.memo pa }

{-
-- | You can flip monad transformers - here for historical interest
inferFlip ::
  UParent.Parent
  -> UPosting.Posting
  -> St.StateT (Maybe E.Entry) (Exceptional Error) Posting
inferFlip pa po = do
  case UPosting.cost po of
    UPosting.Blank -> do
      st <- St.get
      case st of
        Nothing -> lift $ throw CouldNotInferError
        (Just e) -> do
          St.put Nothing
          return $ toPosting pa po e Nothing 
    (UPosting.EntryOnly en) ->
      return $ toPosting pa po en Nothing
    (UPosting.EntryPrice en cpu to) -> let
      pr = Price.priceOnly to cpu en
      in return $ toPosting pa po en (Just pr)

-- | You can flip monad transformers - here for historical interest
runInferFlip ::
  UParent.Parent
  -> Maybe E.Entry
  -> AtLeast2 UPosting.Posting
  -> Exceptional Error (AtLeast2 Posting)
runInferFlip pa me pos = do
  let monErr = St.runStateT k me
      k = Tr.mapM (_inferFlip pa) pos
  case monErr of
    (Exception e) -> throw e
    (Success (a2, finalSt)) -> case finalSt of
      Nothing -> return a2
      (Just _) -> throw UnbalancedError
-}  

