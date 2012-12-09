-- | Copper - the Penny parser.
--
-- The parse functions in this module only accept lists of files
-- rather than individual files because in order to correctly assign
-- the global serials a single function must be able to see all the
-- transactions, not just the transactions in a single file.
module Penny.Copper
  (
  -- * Convenience functions to read and parse files
    parse
  , open
  , openStdin

  -- * Types for things found in ledger files
  , Y.Item(BlankLine, IComment, PricePoint, Transaction)
  , Y.mapItem
  , Y.mapItemA
  , Y.Ledger(Ledger, unLedger)
  , Y.mapLedger
  , Y.mapLedgerA
  , Y.Comment(Comment, unComment)
  , FileContents(FileContents, unFileContents)
  , ErrorMsg (unErrorMsg)

  ) where

import Control.Monad (when, replicateM_)
import Control.Applicative (pure, (*>), (<$>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import qualified Penny.Copper.Parsec as CP

import qualified Penny.Lincoln as L
import qualified Penny.Copper.Types as Y
import qualified System.IO.Error as E
import qualified System.IO as IO

newtype FileContents = FileContents { unFileContents :: X.Text }
                       deriving (Eq, Show)

newtype ErrorMsg = ErrorMsg { unErrorMsg :: X.Text }
                   deriving (Eq, Show)

parseFile ::
  (L.Filename, FileContents)
  -> Ex.Exceptional ErrorMsg Y.Ledger
parseFile (fn, (FileContents c)) =
  let p = fmap (addFileMetadata fn) CP.ledger
      fnStr = X.unpack . L.unFilename $ fn
  in case Parsec.parse p fnStr c of
    Left err -> Ex.throw (ErrorMsg . X.pack . show $ err)
    Right g -> return g

addFileTransaction
  :: L.Filename
  -> L.Transaction
  -> L.GenSerial L.Transaction
addFileTransaction fn t = f <$> L.getSerial
  where
    f ser = L.changeTransaction fam t
      where
        fam = L.Family tl e e []
        e = L.emptyPostingChangeData
        tl = L.emptyTopLineChangeData
             { L.tcFileTransaction =
                Just (Just $ L.FileTransaction ser)
             , L.tcFilename =
                Just (Just fn) }

addFilePosting
  :: L.Transaction
  -> L.GenSerial L.Transaction
addFilePosting t = f <$> (L.mapChildrenA g (L.unTransaction t))
  where
    f fam = L.changeTransaction
            (L.mapParent (const L.emptyTopLineChangeData) fam) t
    g = const $ fmap h L.getSerial
      where h ser = L.emptyPostingChangeData
              { L.pcFilePosting = Just (Just (L.FilePosting ser)) }

addFileMetadataTxn
  :: L.Filename
  -> L.Transaction
  -> Compose L.GenSerial L.GenSerial L.Transaction
addFileMetadataTxn fn t = Compose $ do
  t' <- addFileTransaction fn t
  return (addFilePosting t')

toPostings :: L.Transaction -> [L.Posting]
toPostings = F.toList . L.orphans . L.unTransaction

initCntTxn :: [a] -> L.GenSerial ()
initCntTxn ts = replicateM_ (length ts) L.incrementBack

initCntPstg :: [Y.Item] -> L.GenSerial ()
initCntPstg fs = replicateM_ (length ls) L.incrementBack
  where
    ls = concatMap toPostings . mapMaybe toTxn $ fs

toTxn :: Y.Item -> Maybe L.Transaction
toTxn i = case i of
  Y.Transaction t -> Just t
  _ -> Nothing

addFileMetadata :: L.Filename -> Y.Ledger -> Y.Ledger
addFileMetadata fn a@(Y.Ledger ls) =
  (L.makeSerials . (initCntPstg ls *>))
  . (L.makeSerials . (initCntTxn ls *>))
  . getCompose
  . Y.mapLedgerA (Y.mapItemA pure pure (addFileMetadataTxn fn))
  $ a


addGlobalTransaction
  :: L.Transaction
  -> L.GenSerial L.Transaction
addGlobalTransaction t = f <$> L.getSerial
  where
    f ser = L.changeTransaction fam t
      where
        fam = L.Family tl e e []
        e = L.emptyPostingChangeData
        tl = L.emptyTopLineChangeData
             { L.tcGlobalTransaction =
               Just (Just $ L.GlobalTransaction ser) }

addGlobalPosting
  :: L.Transaction
  -> L.GenSerial L.Transaction
addGlobalPosting t = f <$> (L.mapChildrenA g (L.unTransaction t))
  where
    f fam = L.changeTransaction
            (L.mapParent (const L.emptyTopLineChangeData) fam) t
    g = const $ fmap h L.getSerial
      where
        h ser = L.emptyPostingChangeData
          { L.pcGlobalPosting = Just (Just (L.GlobalPosting ser)) }

addGlobalMetadataTxn ::
  L.Transaction
  -> Compose L.GenSerial L.GenSerial L.Transaction
addGlobalMetadataTxn t = Compose $ do
  t' <- addGlobalTransaction t
  return (addGlobalPosting t')

addGlobalMetadata :: [Y.Ledger] -> Y.Ledger
addGlobalMetadata ls =
  (L.makeSerials . (initCntPstg ls' *>))
  . (L.makeSerials . (initCntTxn ls' *>))
  . getCompose
  . Y.mapLedgerA (Y.mapItemA pure pure addGlobalMetadataTxn)
  $ a
  where
    a@(Y.Ledger ls') = mconcat ls

parse ::
  [(L.Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Y.Ledger
parse ps = fmap addGlobalMetadata $ mapM parseFile ps

-- | Reads and parses the given filename. Does not do anything to
-- handle @-@ arguments; for that, see openStdin. Errors are indicated
-- by throwing an exception.
open :: [String] -> IO Y.Ledger
open = fmap addGlobalMetadata
       . mapM (\s -> getFileContents s >>= parseAndResolve)


getFileContents :: String -> IO (L.Filename, FileContents)
getFileContents s = fmap f (TIO.readFile s)
  where f c = (L.Filename . X.pack $ s, FileContents c)

parseAndResolve :: (L.Filename, FileContents) -> IO Y.Ledger
parseAndResolve p@(L.Filename fn, _) =
  Ex.switch err return $ parseFile p
  where
    err (ErrorMsg x) =
      E.ioError $ E.mkIOError E.userErrorType
        ("could not parse file: " ++ X.unpack x)
        Nothing (Just . X.unpack $ fn)


-- | Reads and parses the given files. If any of the files is @-@,
-- reads standard input. If the list of files is empty, reads standard
-- input. Errors are indicated by throwing an exception.
openStdin :: [String] -> IO Y.Ledger
openStdin ss =
  let ls = if null ss
           then fmap (\x -> [x]) (getFileContentsStdin "-")
           else mapM getFileContentsStdin ss
  in fmap addGlobalMetadata (ls >>= mapM parseAndResolve)

getFileContentsStdin :: String -> IO (L.Filename, FileContents)
getFileContentsStdin s = do
  txt <- if s == "-"
          then do
                isTerm <- IO.hIsTerminalDevice IO.stdin
                when isTerm
                  (IO.hPutStrLn IO.stderr $
                     "warning: reading from standard input, which"
                     ++ "is a terminal.")
                TIO.hGetContents IO.stdin
          else TIO.readFile s
  let fn = L.Filename . X.pack $ if s == "-" then "<stdin>" else s
  return (fn, FileContents txt)
