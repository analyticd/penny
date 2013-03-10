module Penny.Brenner.Clear (mode) where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative (pure)
import Control.Monad (guard, mzero, when)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (mconcat, First(..))
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
import Text.Show.Pretty (ppShow)
import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U


help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " clear clear [options] FIT_FILE LEDGER_FILE..."
  , "Parses all postings that are in FIT_FILE. Then marks all"
  , "postings that are in the FILEs given that correspond to one"
  , "of the postings in the FIT_FILE as being cleared."
  , "Quits if one of the postings found in FIT_FILE is not found"
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
  = APosArg String
  deriving (Eq, Show)

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APosArg s -> Just s }

data Opts = Opts
  { csvLocation :: Y.FitFileLocation
  , ledgerLocations :: [String]
  } deriving Show


mode :: Maybe Y.FitAcct -> MA.Mode (IO ())
mode c = MA.Mode
  { MA.mName = "clear"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [ ]
  , MA.mPosArgs = APosArg
  , MA.mProcess = process c
  , MA.mHelp = help
  }

process :: Maybe Y.FitAcct -> [Arg] -> IO ()
process mayC as = do
  c <- case mayC of
    Just cd -> return cd
    Nothing -> fail $ "no financial institution account given"
               ++ " on command line, and no default financial"
               ++ " institution configured."
  (csv, ls) <- case mapMaybe toPosArg as of
    [] -> fail "clear: you must provide a postings file."
    x:xs -> return (Y.FitFileLocation x, xs)
  let os = Opts csv ls
  runClear c os

runClear :: Y.FitAcct -> Opts -> IO ()
runClear c os = do
  dbList <- U.loadDb (Y.AllowNew False) (Y.dbLocation c)
  let db = M.fromList dbList
      (_, prsr) = Y.parser c
  txns <- fmap (Ex.switch fail return) $ prsr (csvLocation os)
  leds <- C.open (ledgerLocations os)
  toClear <- case mapM (findUNumber db) (concat txns) of
    Nothing -> fail $ "at least one posting was not found in the"
                       ++ " database. Ensure all postings have "
                       ++ "been imported and merged."
    Just ls -> return $ Set.fromList ls
  let (led', left) = changeLedger (Y.pennyAcct c) toClear leds
  when (not (Set.null left))
    (fail $ "some postings were not cleared. "
      ++ "Those not cleared:\n" ++ ppShow left)
  case R.ledger (Y.groupSpecs c) led' of
    Nothing ->
      fail "could not render resulting ledger."
    Just txt -> TIO.putStr txt


-- | Examines an financial institution transaction and the DbMap to
-- find a matching UNumber. Fails if the financial institution
-- transaction is not in the Db.
findUNumber :: Y.DbMap -> Y.Posting -> Maybe Y.UNumber
findUNumber m pstg =
  let atn = Y.fitId pstg
      p ap = Y.fitId ap == atn
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
  :: Y.PennyAcct
  -> Set.Set Y.UNumber
  -> Y.Ledger
  -> (Y.Ledger, Set.Set Y.UNumber)
changeLedger ax s l = St.runState k s
  where
    k = Y.mapLedgerA f l
    f = Y.mapItemA pure pure (changeTxn ax)

changeTxn
  :: Y.PennyAcct
  -> L.Transaction
  -> St.State (Set.Set Y.UNumber) L.Transaction
changeTxn ax t = do
  let fam = L.unTransaction t
      fam' = L.mapParent (const L.emptyTopLineChangeData) fam
  fam'' <- L.mapChildrenA (changePstg ax) fam'
  return $ L.changeTransaction fam'' t


-- | Sees if this posting is a posting in the right account and has a
-- UNumber that needs to be cleared. If so, clears it. If this posting
-- already has a flag, skips it.
changePstg
  :: Y.PennyAcct
  -> L.Posting
  -> St.State (Set.Set Y.UNumber) L.PostingChangeData
changePstg ax p =
  fmap (fromMaybe L.emptyPostingChangeData) . MT.runMaybeT $ do
    guard (L.pAccount p == (Y.unPennyAcct ax))
    let tags = L.pTags p
    un <- maybe mzero return $ parseUNumberFromTags tags
    guard (L.pFlag p == Nothing)
    set <- lift St.get
    guard (Set.member un set)
    lift $ St.put (Set.delete un set)
    return $ L.emptyPostingChangeData
             { L.pcFlag = Just (Just clearedFlag) }

parseUNumberFromTags :: L.Tags -> Maybe Y.UNumber
parseUNumberFromTags =
  getFirst
  . mconcat
  . map First
  . map parseUNumberFromTag
  . L.unTags

parseUNumberFromTag :: L.Tag -> Maybe Y.UNumber
parseUNumberFromTag (L.Tag x) = do
  (f, xs) <- X.uncons x
  guard (f == 'U')
  case reads . X.unpack $ xs of
    (u, ""):[] -> Just (Y.UNumber u)
    _ -> Nothing

