module Penny.Brenner.Amex.Clear (mode) where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative (pure)
import Control.Monad (guard, mzero, when)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Console.MultiArg as MA
import qualified Penny.Lincoln as L
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Trans.Maybe as MT
import Control.Monad.Trans.Class (lift)
import qualified Penny.Copper.Types as Y
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import System.Exit (exitFailure)
import qualified System.IO as IO
import Text.Show.Pretty (ppShow)
import qualified Penny.Brenner.Amex.Types as Y
import qualified Penny.Brenner.Amex.Util as U
import qualified Penny.Brenner.Amex.Parsec as P

help :: String
help = unlines
  [ "usage: amex-clear [options] CSV_FILE LEDGER_FILE..."
  , "Parses all postings that are in CSV_FILE. Then marks all"
  , "postings that are in the FILEs given that correspond to one"
  , "of the postings in the CSV_FILE as being cleared."
  , "Quits if one of the postings found in CSV_FILE is not found"
  , "in the database, if one of the postings in the database"
  , "is not found in one of the FILEs, or if any of the postings found"
  , "in one of the FILEs already has a flag."
  , ""
  , "Results are printed to standard output. If no FILE, or FILE is \"-\","
  , "read standard input."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]

data Arg
  = AHelp
  | APosArg String
  deriving (Eq, Show)

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APosArg s -> Just s; _ -> Nothing }

data Opts = Opts
  { csvLocation :: Y.CSVLocation
  , ledgerLocations :: [String]
  } deriving Show


mode :: Y.Card -> MA.Mode String (Ex.Exceptional String (IO ()))
mode c = MA.Mode
  { MA.mId = "clear"
  , MA.mName = "clear"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp) ]
  , MA.mPosArgs = APosArg
  , MA.mProcess = process c
  }

process :: Y.Card -> [Arg] -> Ex.Exceptional String (IO ())
process c as =
  if any (== AHelp) as
  then return $ putStrLn help
  else do
    (csv, ls) <- case mapMaybe toPosArg as of
      [] -> Ex.throw $ "clear: you must provide a CSV file."
      x:xs -> return (Y.CSVLocation x, xs)
    let os = Opts csv ls
    return $ runClear c os

fatal :: String -> IO a
fatal s = do
  IO.hPutStrLn IO.stderr $ "clear: error: " ++ s
  exitFailure

runClear :: Y.Card -> Opts -> IO ()
runClear c os = do
  db <- fmap M.fromList
        $ U.loadDb (Y.AllowNew False) (Y.dbLocation c)
  txns <- P.loadIncoming (csvLocation os)
  leds <- C.openStdin (ledgerLocations os)
  toClear <- case mapM (findUNumber db) txns of
    Nothing -> fatal $ "at least one posting was not found in the"
                       ++ " database. Ensure all postings have "
                       ++ "been imported and merged."
    Just ls -> return $ Set.fromList ls
  let (led', left) = changeLedger (Y.amexAcct c) toClear leds
  when (not (Set.null left))
    (fatal $ "some Amex postings were not cleared. "
      ++ "Those not cleared:\n" ++ ppShow left)
  case R.ledger (Y.groupSpecs c) led' of
    Nothing ->
      fatal "could not render resulting ledger."
    Just txt -> TIO.putStr txt


-- | Examines an AmexTxn and the DbMap to find a matching
-- UNumber. Fails if the AmexTxn is not in the Db.
findUNumber :: Y.DbMap -> Y.Posting -> Maybe Y.UNumber
findUNumber m pstg =
  let atn = Y.amexId pstg
      p ap = Y.amexId ap == atn
      filteredMap = M.filter p m
      ls = M.toList filteredMap
  in case ls of
      (n, _):[] -> Just n
      _ -> Nothing


clearedFlag :: L.Flag
clearedFlag = L.Flag . X.singleton $ 'C'

-- | Changes a ledger to clear postings. Returns postings still not
-- cleared.
changeLedger
  :: Y.AmexAcct
  -> Set.Set Y.UNumber
  -> Y.Ledger
  -> (Y.Ledger, Set.Set Y.UNumber)
changeLedger ax s l = St.runState k s
  where
    k = Y.mapLedgerA f l
    f = Y.mapItemA pure pure (changeTxn ax)

changeTxn
  :: Y.AmexAcct
  -> L.Transaction
  -> St.State (Set.Set Y.UNumber) L.Transaction
changeTxn ax t = do
  let fam = L.unTransaction t
      fam' = L.mapParent (const L.emptyTopLineChangeData) fam
  fam'' <- L.mapChildrenA (changePstg ax) fam'
  return $ L.changeTransaction fam'' t


-- | Sees if this posting is an Amex posting and has a UNumber that
-- needs to be cleared. If so, clears it. If this posting already has
-- a flag, skips it.
changePstg
  :: Y.AmexAcct
  -> L.Posting
  -> St.State (Set.Set Y.UNumber) L.PostingChangeData
changePstg ax p =
  fmap (fromMaybe L.emptyPostingChangeData) . MT.runMaybeT $ do
    guard (L.pAccount p == (Y.unAmexAcct ax))
    mayUn <- maybe mzero return $ L.pNumber p
    un <- maybe mzero return $ parseUNumber mayUn
    guard (L.pFlag p == Nothing)
    set <- lift St.get
    guard (Set.member un set)
    lift $ St.put (Set.delete un set)
    return $ L.emptyPostingChangeData
             { L.pcFlag = Just (Just clearedFlag) }

parseUNumber :: L.Number -> Maybe Y.UNumber
parseUNumber (L.Number x) = do
  (f, xs) <- X.uncons x
  guard (f == 'U')
  case reads . X.unpack $ xs of
    (u, ""):[] -> Just (Y.UNumber u)
    _ -> Nothing

