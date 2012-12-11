-- | amex-import parses downloaded data from the Amex website and
-- imports it to the database.
--
-- Usage:
--
-- amex-import [options] CSV_FILE
--
-- where CSV_FILE is the file downloaded from the Amex website.
--
-- Options:
--
-- -d, --db FILE - Location of database file. If omitted, defaults to
-- hard-coded location.
--
-- -n, --new - Allows creation of new database. Without this option,
-- if the database file is not found, quits with an error.

module Penny.Brenner.Amex.Import where

import Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)
import qualified System.Console.MultiArg as MA
import qualified Penny.Brenner.Amex.Types as Y
import qualified Penny.Brenner.Amex.Util as U
import qualified Penny.Brenner.Amex.Parsec as P

data Arg
  = AHelp
  | ACSV String
  | AAllowNew
  deriving (Eq, Show)

toCSV :: Arg -> Maybe String
toCSV a = case a of
  ACSV s -> Just s
  _ -> Nothing

data ImportOpts = ImportOpts
  { csvFile :: Y.CSVLocation
  , allowNew :: Y.AllowNew
  }

mode
  :: Y.DbLocation
  -> MA.Mode String (Ex.Exceptional String (IO ()))
mode dbLoc = MA.Mode
  { MA.mId = "import"

  , MA.mName = "import"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts =
      [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp)
      , MA.OptSpec ["new"] "n" (MA.NoArg AAllowNew)
      ]
  , MA.mPosArgs = ACSV
  , MA.mProcess = processor dbLoc
  }


processor :: Y.DbLocation -> [Arg] -> Ex.Exceptional String (IO ())
processor dbLoc as = do
  let err s = Ex.throw $ "import: " ++ s
  if any (== AHelp) as
    then return $ putStrLn help
    else do
      loc <- case mapMaybe toCSV as of
        [] -> err "you must provide a CSV file to read"
        x:[] -> return (Y.CSVLocation x)
        _ -> err "you cannot provide more than one CSV file to read"
      let aNew = Y.AllowNew $ any (== AAllowNew) as
      return $ doImport dbLoc (ImportOpts loc aNew)


-- | Appends new Amex transactions to the existing list.
appendNew
  :: [(Y.UNumber, Y.Posting)]
  -- ^ Existing transactions

  -> [Y.Posting]
  -- ^ New transactions

  -> ([(Y.UNumber, Y.Posting)], Int)
  -- ^ New list, and number of transactions added

appendNew db new = (db ++ newWithU, length newWithU)
  where
    nextUNum = if null db
               then 0
               else (Y.unUNumber . maximum . map fst $ db) + 1
    currAmexIds = map (Y.amexId . snd) db
    isNew p = not (any (== Y.amexId p) currAmexIds)
    newPstgs = filter isNew new
    mkPair i p = (Y.UNumber i, p)
    newWithU = zipWith mkPair [nextUNum..] newPstgs


doImport :: Y.DbLocation -> ImportOpts -> IO ()
doImport dbLoc os = do
  txnsOld <- U.loadDb (allowNew os) dbLoc
  ins <- P.loadIncoming . csvFile $ os
  let (new, len) = appendNew txnsOld ins
  U.saveDb dbLoc new
  putStrLn $ "imported " ++ show len ++ " new transactions."

help :: String
help = unlines
  [ "amex-import [options] CSV_FILE"
  , "where CSV_FILE is the file downloaded from the Amex website."
  , ""
  , "Options:"
  , ""
  , "-d, --db FILE - Location of database file. If omitted, defaults to"
  , "hard-coded location."
  , ""
  , "-n, --new - Allows creation of new database. Without this option,"
  , "if the database file is not found, quits with an error."
  , ""
  , "-h, --help - Show this help."
  , ""
  ]

