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
  , Y.Ledger(Ledger, unLedger)
  , Y.Comment(Comment, unComment)
  , FileContents(FileContents, unFileContents)
  , ErrorMsg (unErrorMsg)

  ) where

import Control.Monad (when)
import Control.Applicative ((<*>), (<$), pure)
import qualified Control.Monad.Exception.Synchronous as Ex
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

addTopLineFileMetadata
  :: L.Filename
  -> Y.Ledger
  -> Y.Ledger
addTopLineFileMetadata fn (Y.Ledger ls) =
  let incr t = t <$ L.incrementBack
      incrementCts = Y.mapLedgerA (Y.mapItemA pure pure incr) ls
      assign t = fmap f L.get
        where
          f ser = L.emptyTopLineChangeData
                    { L.tcMeta = Just tlm }


addFileMetadata ::
  L.Filename
  -> Y.Ledger
  -> Y.Ledger
addFileMetadata fn (Y.Ledger ls) =
  let eis = map toEiItem ls
      procTop s m =
        m { L.fileTransaction = Just (L.FileTransaction s)
          , L.filename = Just fn }
      procPstg s m =
        m { L.filePosting = Just (L.FilePosting s) }
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in Y.Ledger is'


addGlobalMetadata :: [Y.Ledger] -> Y.Ledger
addGlobalMetadata lss =
  let ls = concat . map Y.unLedger $ lss
      procTop s m =
        m { L.globalTransaction = Just (L.GlobalTransaction s) }
      procPstg s m =
        m { L.globalPosting = Just (L.GlobalPosting s) }
      eis = map toEiItem ls
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in Y.Ledger is'

parse ::
  [(L.Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Y.Ledger
parse ps = fmap addGlobalMetadata $ mapM parseFile ps

data Other = OPrice L.PricePoint
             | OComment Y.Comment
             | OBlankLine
             deriving Show

type EiItem = Either Other L.Transaction

toEiItem :: Y.Item -> EiItem
toEiItem i = case i of
  Y.Transaction t -> Right t
  Y.PricePoint p -> Left (OPrice p)
  Y.IComment c -> Left (OComment c)
  Y.BlankLine -> Left OBlankLine

fromEiItem :: EiItem -> Y.Item
fromEiItem i = case i of
  Left l -> case l of
    OPrice p -> Y.PricePoint p
    OComment c -> Y.IComment c
    OBlankLine -> Y.BlankLine
  Right t -> Y.Transaction t

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
