module Penny.Brenner.Clear (mode) where

import Control.Applicative
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad (guard, mzero, when)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (mconcat, First(..))
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as Tr
import qualified System.Console.MultiArg as MA
import qualified Penny.Lincoln as L
import qualified Penny.Steel.Sums as S
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Trans.Maybe as MT
import Control.Monad.Trans.Class (lift)
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
  | AHelp (IO ())

toPosArg :: Arg -> Maybe String
toPosArg a = case a of { APosArg s -> Just s; _ -> Nothing }

data Opts = Opts
  { csvLocation :: Y.FitFileLocation
  , ledgerLocations :: [String]
  } deriving Show


mode :: MA.Mode (Y.FitAcct -> IO ())
mode = MA.Mode
  { MA.mName = "clear"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [ fmap AHelp (U.help help) ]
  , MA.mPosArgs = return . APosArg
  , MA.mProcess = process
  , MA.mHelp = help
  }

process :: [Arg] -> Y.FitAcct -> IO ()
process as c = do
  U.printHelp (\a -> case a of { AHelp h -> Just h; _ -> Nothing }) as
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
      led'' = map C.stripMeta led'
  when (not (Set.null left))
    (fail $ "some postings were not cleared. "
      ++ "Those not cleared:\n" ++ ppShow left)
  case mapM (R.item (Y.groupSpecs c)) led'' of
    Nothing ->
      fail "could not render resulting ledger."
    Just txts -> mapM_ TIO.putStr txts


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
  -> [C.LedgerItem]
  -> ([C.LedgerItem], Set.Set Y.UNumber)
changeLedger ax s l = St.runState k s
  where
    k = mapM f l
    f x = case x of
      S.S4a t -> fmap S.S4a $ changeTxn ax t
      S.S4b z -> fmap S.S4b $ return z
      S.S4c z -> fmap S.S4c $ return z
      S.S4d z -> fmap S.S4d $ return z

changeTxn
  :: Y.PennyAcct
  -> L.Transaction
  -> St.State (Set.Set Y.UNumber) L.Transaction
changeTxn ax (L.Transaction (tld, d)) =
  (\tl es -> L.Transaction (tl, es))
  <$> pure tld
  <*> Tr.mapM (changePstg ax) d


-- | Sees if this posting is a posting in the right account and has a
-- UNumber that needs to be cleared. If so, clears it. If this posting
-- already has a flag, skips it.
changePstg
  :: Y.PennyAcct
  -> L.PostingData
  -> St.State (Set.Set Y.UNumber) L.PostingData
changePstg ax p =
  fmap (fromMaybe p) . MT.runMaybeT $ do
    let c = L.pdCore p
    guard (L.pAccount c == (Y.unPennyAcct ax))
    let tags = L.pTags c
    un <- maybe mzero return $ parseUNumberFromTags tags
    guard (L.pFlag c == Nothing)
    set <- lift St.get
    guard (Set.member un set)
    lift $ St.put (Set.delete un set)
    let c' = c { L.pFlag = Just clearedFlag }
    return $ p { L.pdCore = c' }

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

