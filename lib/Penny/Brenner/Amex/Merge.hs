module Penny.Brenner.Amex.Merge where

import Control.Applicative ((<*), pure)
import Control.Monad (guard, when)
import qualified Control.Monad.Trans.State as St
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.IO.Strict as Strict
import qualified System.IO as IO
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Parsec as CP
import System.Exit (exitSuccess, exitFailure)
import qualified Text.Parsec as Psc
import Text.Show.Pretty (ppShow)

data Arg
  = AHelp
  | APos String
  deriving (Eq, Show)

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APos s -> Just s; _ -> Nothing }

mode :: MA.Mode String (Ex.Exceptional String (IO ()))
mode = MA.Mode
  { MA.mId = "merge"
  , MA.mName = "merge"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts =
    [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp)
    ]
  , MA.mPosArgs = APos
  , MA.mProcess = processor
  }

processor :: [Arg] -> Ex.Exceptional a (IO ())
processor as = return $
  if any (== AHelp) as
  then putStrLn help
  else doMerge (mapMaybe toPosArg as)

doMerge :: [String] -> IO ()
doMerge = undefined

help :: String
help = unlines
  [ "penny-amex merge: merges new transactions from database to ledger file."
  , "usage: penny-amex merge [options] FILE..."
  , "Results are printed to standard output. If no FILE, or if FILE is -,"
  , "read standard input."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]

{-
data MergeOpts = MergeOpts
  { dbLocation :: String
  , ledgers :: [String]
  } deriving (Eq, Show)

-- | Removes all AmexTxns that already have a Penny posting with the
-- correct uNumber.
filterDb :: A.DbMap -> C.Ledger -> A.DbMap
filterDb m l = M.difference m ml
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
    isAmex p = Q.account p == A.amexAcct
    toUNum p = do
      (L.Number n) <- Q.number p
      (f, r) <- X.uncons n
      guard (f == 'U')
      case reads . X.unpack $ r of
        (n, ""):[] -> return n
        _ -> Nothing


changeTransaction :: L.Transaction -> St.State A.DbMap L.Transaction
changeTransaction txn = do
  let fam = L.unTransaction txn
      fam' = L.mapParent (const L.emptyTopLineChangeData) fam
  fam'' <- L.mapChildrenA (inspectAndChange txn) fam'
  return $ L.changeTransaction fam'' txn


-- | Changes a single Item.
changeItem :: C.Item -> St.State A.DbMap C.Item
changeItem = C.mapItemA pure pure changeTransaction


-- | Changes all postings that match an AmexTxn to assign them the
-- proper UNumber. Returns a list of changed items, and the DbMap of
-- still-unassigned AmexTxns.
changeItems :: C.Ledger -> A.DbMap -> (C.Ledger, A.DbMap)
changeItems l = St.runState (C.mapLedgerA changeItem l)


-- | Creates new transactions for all the items remaining in the
-- DbMap. Appends a blank line after each one.
createTransactions :: A.DbMap -> [C.Item]
createTransactions =
  concatMap (\i -> [i, C.BlankLine])
  . map C.Transaction
  . map newTransaction
  . M.assocs

-- | Creates a new transaction corresponding to a given AmexTxn. Uses
-- the Amex payee if that string is non empty; otherwise, uses the
-- Amex description for the payee.
newTransaction :: (A.UNumber, (A.AmexTxn, L.Entry)) -> L.Transaction
newTransaction (u, (a, e)) = L.rTransaction rt
  where
    rt = L.RTransaction
      { L.rtCommodity = A.currency
      , L.rtSide = Just L.CommodityOnLeft
      , L.rtSpaceBetween = Just L.NoSpaceBetween
      , L.rtDrCr = L.drCr e
      , L.rtTopLine = tl
      , L.rtPosting = p1
      , L.rtMorePostings = []
      , L.rtIPosting = p2
      }
    tl = (U.emptyTopLine (L.dateTimeMidnightUTC . A.date $ a))
         { U.tPayee = Just (L.Payee . X.pack $ pa) }
    pa = if null . A.payee $ a
         then A.description a
         else A.payee a
    p1 = (U.emptyRPosting A.amexAcct (L.qty . L.amount $ e))
          { U.rNumber = Just $ newLincolnUNumber u }
    p2 = (U.emptyIPosting A.defaultAcct)


-- | Inspects a posting to see if it is an Amex posting and, if so,
-- whether it matches one of the remaining AmexTxns. If so, then
-- changes the transaction's UNumber, and remove that UNumber from the
-- DbMap. If the posting alreay has a Number (UNumber or otherwise)
-- skips it.
inspectAndChange
  :: L.Transaction
  -> L.Posting
  -> St.State A.DbMap L.PostingChangeData
inspectAndChange t p = do
  m <- St.get
  case findMatch t p m of
    Nothing -> return L.emptyPostingChangeData
    Just (n, m') ->
      let pcd = L.emptyPostingChangeData
                  { L.pcNumber = Just (Just (newLincolnUNumber n)) }
      in St.put m' >> return pcd

newLincolnUNumber :: A.UNumber -> L.Number
newLincolnUNumber a = L.Number ('U' `X.cons` (X.pack . show $ a))

-- | Searches a DbMap for an AmexTxn that matches a given posting. If
-- a match is found, returns the matching UNumber and a new DbMap that
-- has the match removed.
findMatch
  :: L.Transaction
  -> L.Posting
  -> A.DbMap
  -> Maybe (A.UNumber, A.DbMap)
findMatch t p m = fmap toPair findResult
  where
    findResult = find (amexTxnMatches t p) . M.assocs $ m
    toPair (u, (a, _)) = (u, M.delete u m)


-- | Does the given AmexTxn match this posting? Makes sure that the
-- account, quantity, date, and DrCr match, and that the posting does
-- not have a number (it's OK if the transaction has a number.)
amexTxnMatches
  :: L.Transaction
  -> L.Posting
  -> (a, (A.AmexTxn, L.Entry))
  -> Bool
amexTxnMatches t p (_, (a, e)) = mA && noFlag && mQ && mDC && mDate
  where
    mA = L.pAccount p == A.amexAcct
    mQ = L.equivalent (L.qty . L.amount . L.pEntry $ p)
                      (L.qty . L.amount $ e)
    mDC = (L.drCr e) == (L.drCr . L.pEntry $ p)
    (L.Family tl _ _ _) = L.unTransaction t
    mDate = (L.day . L.tDateTime $ tl) == (A.date a)
    noFlag = isNothing . L.pNumber $ p


main :: IO ()
main = do
  os <- fmap parseOpts MA.getArgs
  mergeOpts <- case os of
    NeedsHelp -> putStrLn help >> exitSuccess
    DoMerge o -> return o
  db <- A.readDb (dbLocation mergeOpts)
  l <- C.openStdin (ledgers mergeOpts)
  let (l', db') = changeItems l (filterDb db l)
      newTxns = createTransactions db'
      final = C.Ledger (C.unLedger l' ++ newTxns)
  case R.ledger A.groupSpecs final of
    Nothing -> do
      IO.hPutStrLn IO.stderr
        "Could not render final ledger. Internal representation: "
      IO.hPutStrLn IO.stderr (ppShow final)
      exitFailure
    Just x -> TIO.putStr x
-}
