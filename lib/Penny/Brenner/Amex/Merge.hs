module Penny.Brenner.Amex.Merge (mode) where

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
import Text.Show.Pretty (ppShow)
import qualified Penny.Brenner.Amex.Types as Y
import qualified Penny.Brenner.Amex.Parsec as P
import qualified Penny.Brenner.Amex.Util as U

data Arg
  = AHelp
  | APos String
  deriving (Eq, Show)

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APos s -> Just s; _ -> Nothing }

mode :: Y.Card -> MA.Mode String (Ex.Exceptional String (IO ()))
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

processor :: Y.Card -> [Arg] -> Ex.Exceptional a (IO ())
processor c as = return $
  if any (== AHelp) as
  then putStrLn help
  else doMerge c (mapMaybe toPosArg as)

doMerge :: Y.Card -> [String] -> IO ()
doMerge (Y.Card dbLoc ax df cy gs) ss = do
  dbLs <- U.loadDb (Y.AllowNew False) dbLoc
  l <- C.openStdin ss
  let (l', db') = changeItems cy ax
                  l (filterDb ax (M.fromList dbLs) l)
      newTxns = createTransactions cy ax df db'
      final = C.Ledger (C.unLedger l' ++ newTxns)
  case R.ledger gs final of
    Nothing -> do
      IO.hPutStrLn IO.stderr
        "Could not render final ledger. Internal representation: "
      IO.hPutStrLn IO.stderr (ppShow final)
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

-- | Removes all AmexTxns that already have a Penny posting with the
-- correct uNumber.
filterDb :: Y.AmexAcct -> Y.DbMap -> C.Ledger -> Y.DbMap
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
    isAmex p = Q.account p == (Y.unAmexAcct ax)
    toUNum p = do
      (L.Number n) <- Q.number p
      (f, r) <- X.uncons n
      guard (f == 'U')
      case reads . X.unpack $ r of
        (x, ""):[] -> return $ Y.UNumber x
        _ -> Nothing

-- | Changes a single Item.
changeItem
  :: Y.Currency
  -> Y.AmexAcct
  -> C.Item
  -> St.State Y.DbMap C.Item
changeItem cy ax =
  C.mapItemA pure pure (changeTransaction cy ax)


-- | Changes all postings that match an AmexTxn to assign them the
-- proper UNumber. Returns a list of changed items, and the DbMap of
-- still-unassigned AmexTxns.
changeItems
  :: Y.Currency
  -> Y.AmexAcct
  -> C.Ledger
  -> Y.DbMap
  -> (C.Ledger, Y.DbMap)
changeItems cy ax l =
  St.runState (C.mapLedgerA (changeItem cy ax) l)


changeTransaction
  :: Y.Currency
  -> Y.AmexAcct
  -> L.Transaction
  -> St.State Y.DbMap L.Transaction
changeTransaction cy ax txn = do
  let fam = L.unTransaction txn
      fam' = L.mapParent (const L.emptyTopLineChangeData) fam
  fam'' <- L.mapChildrenA (inspectAndChange cy ax txn) fam'
  return $ L.changeTransaction fam'' txn

-- | Inspects a posting to see if it is an Amex posting and, if so,
-- whether it matches one of the remaining AmexTxns. If so, then
-- changes the transaction's UNumber, and remove that UNumber from the
-- DbMap. If the posting alreay has a Number (UNumber or otherwise)
-- skips it.
inspectAndChange
  :: Y.Currency
  -> Y.AmexAcct
  -> L.Transaction
  -> L.Posting
  -> St.State Y.DbMap L.PostingChangeData
inspectAndChange cy ax t p = do
  m <- St.get
  case findMatch cy ax t p m of
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
  :: Y.Currency
  -> Y.AmexAcct
  -> L.Transaction
  -> L.Posting
  -> Y.DbMap
  -> Maybe (Y.UNumber, Y.DbMap)
findMatch cy ax t p m = fmap toResult findResult
  where
    toPair (u, pstg) = (u, (pstg, P.parseEntry cy pstg))
    dbLs = map toPair . M.assocs $ m
    findResult = find (amexTxnMatches ax t p) dbLs
    toResult (u, (_, _)) = (u, M.delete u m)

-- | Does the given AmexTxn match this posting? Makes sure that the
-- account, quantity, date, and DrCr match, and that the posting does
-- not have a number (it's OK if the transaction has a number.)
amexTxnMatches
  :: Y.AmexAcct
  -> L.Transaction
  -> L.Posting
  -> (a, (Y.Posting, L.Entry))
  -> Bool
amexTxnMatches ax t p (_, (a, e)) = mA && noFlag && mQ && mDC && mDate
  where
    mA = L.pAccount p == Y.unAmexAcct ax
    mQ = L.equivalent (L.qty . L.amount . L.pEntry $ p)
                      (L.qty . L.amount $ e)
    mDC = (L.drCr e) == (L.drCr . L.pEntry $ p)
    (L.Family tl _ _ _) = L.unTransaction t
    mDate = (L.day . L.tDateTime $ tl) == (Y.unDate . Y.date $ a)
    noFlag = isNothing . L.pNumber $ p


-- | Creates a new transaction corresponding to a given AmexTxn. Uses
-- the Amex payee if that string is non empty; otherwise, uses the
-- Amex description for the payee.
newTransaction
  :: Y.Currency
  -> Y.AmexAcct
  -> Y.DefaultAcct
  -> (Y.UNumber, (Y.Posting, L.Entry))
  -> L.Transaction
newTransaction cy ax df (u, (a, e)) = L.rTransaction rt
  where
    rt = L.RTransaction
      { L.rtCommodity = Y.unCurrency cy
      , L.rtSide = Just L.CommodityOnLeft
      , L.rtSpaceBetween = Just L.NoSpaceBetween
      , L.rtDrCr = L.drCr e
      , L.rtTopLine = tl
      , L.rtPosting = p1
      , L.rtMorePostings = []
      , L.rtIPosting = p2
      }
    tl = (U.emptyTopLine ( L.dateTimeMidnightUTC . Y.unDate
                           . Y.date $ a))
         { U.tPayee = Just (L.Payee . X.pack $ pa) }
    pa = if null . Y.unPayee . Y.payee $ a
         then Y.unDesc . Y.desc $ a
         else Y.unPayee . Y.payee $ a
    p1 = (U.emptyRPosting (Y.unAmexAcct ax) (L.qty . L.amount $ e))
          { U.rNumber = Just $ newLincolnUNumber u }
    p2 = U.emptyIPosting (Y.unDefaultAcct df)

-- | Creates new transactions for all the items remaining in the
-- DbMap. Appends a blank line after each one.
createTransactions
  :: Y.Currency
  -> Y.AmexAcct
  -> Y.DefaultAcct
  -> Y.DbMap
  -> [C.Item]
createTransactions cy ax df =
  concatMap (\i -> [i, C.BlankLine])
  . map C.Transaction
  . map (newTransaction cy ax df)
  . map (\(u, p) -> (u, (p, P.parseEntry cy p)))
  . M.assocs

