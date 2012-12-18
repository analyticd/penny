module Penny.Brenner.Merge (mode) where

import Control.Applicative (pure)
import Control.Monad (guard)
import qualified Control.Monad.Trans.State as St
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isNothing)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.IO as IO
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Queries as Q
import System.Exit (exitFailure)
import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U

data Arg
  = AHelp
  | APos String
  deriving (Eq, Show)

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APos s -> Just s; _ -> Nothing }

mode :: Y.FitAcct -> MA.Mode String (Ex.Exceptional String (IO ()))
mode c = MA.Mode
  { MA.mId = "merge"
  , MA.mName = "merge"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts =
    [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp)
    ]
  , MA.mPosArgs = APos
  , MA.mProcess = processor c
  }

processor :: Y.FitAcct -> [Arg] -> Ex.Exceptional a (IO ())
processor c as = return $
  if any (== AHelp) as
  then putStrLn help
  else doMerge c (mapMaybe toPosArg as)

doMerge :: Y.FitAcct -> [String] -> IO ()
doMerge acct ss = do
  dbLs <- U.quitOnError
          $ U.loadDb (Y.AllowNew False) (Y.dbLocation acct)
  l <- C.openStdin ss
  let dbWithEntry = fmap (pairWithEntry acct) . M.fromList $ dbLs
      (l', db') = changeItems acct
                  l (filterDb (Y.pennyAcct acct) dbWithEntry l)
      newTxns = createTransactions acct db'
      final = C.Ledger (C.unLedger l' ++ newTxns)
  case R.ledger (Y.groupSpecs acct) final of
    Nothing -> do
      IO.hPutStrLn IO.stderr "Could not render final ledger."
      exitFailure
    Just x -> TIO.putStr x


help :: String
help = unlines
  [ "penny-amex merge: merges new transactions from database"
  , "to ledger file."
  , "usage: penny-amex merge [options] FILE..."
  , "Results are printed to standard output. If no FILE, or if FILE is -,"
  , "read standard input."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]

-- | Removes all Brenner postings that already have a Penny posting
-- with the correct uNumber.
filterDb :: Y.PennyAcct -> DbWithEntry -> C.Ledger -> DbWithEntry
filterDb ax m l = M.difference m ml
  where
    ml = M.fromList
       . flip zip (repeat ())
       . mapMaybe toUNum
       . filter isAmex
       . concatMap L.postFam
       . mapMaybe toTxn
       . C.unLedger
       $ l
    toTxn t = case t of
      C.Transaction x -> Just x
      _ -> Nothing
    isAmex p = Q.account p == (Y.unPennyAcct ax)
    toUNum p = do
      (L.Number n) <- Q.number p
      (f, r) <- X.uncons n
      guard (f == 'U')
      case reads . X.unpack $ r of
        (x, ""):[] -> return $ Y.UNumber x
        _ -> Nothing

-- | Changes a single Item.
changeItem
  :: Y.FitAcct
  -> C.Item
  -> St.State DbWithEntry C.Item
changeItem acct =
  C.mapItemA pure pure (changeTransaction acct)


-- | Changes all postings that match an AmexTxn to assign them the
-- proper UNumber. Returns a list of changed items, and the DbMap of
-- still-unassigned AmexTxns.
changeItems
  :: Y.FitAcct
  -> C.Ledger
  -> DbWithEntry
  -> (C.Ledger, DbWithEntry)
changeItems acct l =
  St.runState (C.mapLedgerA (changeItem acct) l)


changeTransaction
  :: Y.FitAcct
  -> L.Transaction
  -> St.State DbWithEntry L.Transaction
changeTransaction acct txn = do
  let fam = L.unTransaction txn
      fam' = L.mapParent (const L.emptyTopLineChangeData) fam
  fam'' <- L.mapChildrenA (inspectAndChange acct txn) fam'
  return $ L.changeTransaction fam'' txn

-- | Inspects a posting to see if it is an Amex posting and, if so,
-- whether it matches one of the remaining AmexTxns. If so, then
-- changes the transaction's UNumber, and remove that UNumber from the
-- DbMap. If the posting alreay has a Number (UNumber or otherwise)
-- skips it.
inspectAndChange
  :: Y.FitAcct
  -> L.Transaction
  -> L.Posting
  -> St.State DbWithEntry L.PostingChangeData
inspectAndChange acct t p = do
  m <- St.get
  case findMatch acct t p m of
    Nothing -> return L.emptyPostingChangeData
    Just (n, m') ->
      let pcd = L.emptyPostingChangeData
                  { L.pcNumber = Just (Just (newLincolnUNumber n)) }
      in St.put m' >> return pcd

newLincolnUNumber :: Y.UNumber -> L.Number
newLincolnUNumber a =
  L.Number ('U' `X.cons` (X.pack . show . Y.unUNumber $ a))


-- | Searches a DbMap for an AmexTxn that matches a given posting. If
-- a match is found, returns the matching UNumber and a new DbMap that
-- has the match removed.
findMatch
  :: Y.FitAcct
  -> L.Transaction
  -> L.Posting
  -> DbWithEntry
  -> Maybe (Y.UNumber, DbWithEntry)
findMatch acct t p m = fmap toResult findResult
  where
    findResult = find (pennyTxnMatches acct t p)
                 . M.toList $ m
    toResult (u, (_, _)) = (u, M.delete u m)

-- | Pairs each association in a DbMap with an Entry representing the
-- transaction's entry in the ledger.
pairWithEntry :: Y.FitAcct -> Y.Posting -> (Y.Posting, L.Entry)
pairWithEntry acct p = (p, en)
  where
    en = L.Entry dc (L.Amount qty cty (Just (Y.side acct))
                                      (Just (Y.spaceBetween acct)))
    dc = Y.translate (Y.incDec p) (Y.translator acct)
    qty = U.parseQty (Y.amount p)
    cty = Y.unCurrency . Y.currency $ acct

type DbWithEntry = M.Map Y.UNumber (Y.Posting, L.Entry)

-- | Does the given Penny transaction match this posting? Makes sure
-- that the account, quantity, date, commodity, and DrCr match, and
-- that the posting does not have a number (it's OK if the transaction
-- has a number.)
pennyTxnMatches
  :: Y.FitAcct
  -> L.Transaction
  -> L.Posting
  -> (a, (Y.Posting, L.Entry))
  -> Bool
pennyTxnMatches acct t p (_, (a, e)) =
  mA && noFlag && mQ && mDC && mDate && mCmdty
  where
    mA = L.pAccount p == (Y.unPennyAcct . Y.pennyAcct $ acct)
    mQ = L.equivalent (L.qty . L.amount . L.pEntry $ p)
                      (L.qty . L.amount $ e)
    mDC = (L.drCr e) == (L.drCr . L.pEntry $ p)
    (L.Family tl _ _ _) = L.unTransaction t
    mDate = (L.day . L.tDateTime $ tl) == (Y.unDate . Y.date $ a)
    noFlag = isNothing . L.pNumber $ p
    mCmdty = (L.commodity . L.amount $ e)
              == (Y.unCurrency . Y.currency $ acct)


-- | Creates a new transaction corresponding to a given AmexTxn. Uses
-- the Amex payee if that string is non empty; otherwise, uses the
-- Amex description for the payee.
newTransaction
  :: Y.FitAcct
  -> (Y.UNumber, (Y.Posting, L.Entry))
  -> L.Transaction
newTransaction acct (u, (a, e)) = L.rTransaction rt
  where
    rt = L.RTransaction
      { L.rtCommodity = Y.unCurrency . Y.currency $ acct
      , L.rtSide = Just . Y.side $ acct
      , L.rtSpaceBetween = Just . Y.spaceBetween $ acct
      , L.rtDrCr = L.drCr e
      , L.rtTopLine = tl
      , L.rtPosting = p1
      , L.rtMorePostings = []
      , L.rtIPosting = p2
      }
    tl = (U.emptyTopLine ( L.dateTimeMidnightUTC . Y.unDate
                           . Y.date $ a))
         { U.tPayee = Just (L.Payee pa) }
    pa = if X.null . Y.unPayee . Y.payee $ a
         then Y.unDesc . Y.desc $ a
         else Y.unPayee . Y.payee $ a
    pennyAcct = Y.unPennyAcct . Y.pennyAcct $ acct
    p1 = (U.emptyRPosting pennyAcct (L.qty . L.amount $ e))
          { U.rNumber = Just $ newLincolnUNumber u }
    p2 = U.emptyIPosting (Y.unDefaultAcct . Y.defaultAcct $ acct)

-- | Creates new transactions for all the items remaining in the
-- DbMap. Appends a blank line after each one.
createTransactions
  :: Y.FitAcct
  -> DbWithEntry
  -> [C.Item]
createTransactions acct =
  concatMap (\i -> [i, C.BlankLine])
  . map C.Transaction
  . map (newTransaction acct)
  . M.assocs

