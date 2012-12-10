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
import Control.Monad (when)
import qualified Data.Set as Set
import qualified System.Console.MultiArg as MA
import System.Console.MultiArg.Prim (Parser)
import qualified System.Console.MultiArg.Prim as MAP
import qualified System.Console.MultiArg.Combinator as MAC
import System.Exit (exitSuccess)
import qualified Penny.Brenner.Amex.Types as Y

data Arg
  = AHelp
  | ACSV String
  | AAllowNew

data Opts
  = ShowHelp
  | DoImport ImportOpts

data ImportOpts = ImportOpts
  { csvFile :: Y.CSVLocation
  , allowNew :: Y.AllowNew
  }


parseOpts :: Y.Card -> Parser 
parseOpts cd = do
  

parseOpts :: [String] -> Opts
parseOpts ss =
  Ex.switch err mkO
  . MA.parse MA.Intersperse os posArg
  $ ss
  where
    posArg s o = o { csvFile = Just s }
    err e = error $ "could not parse command line: " ++ show e
    mkO = foldl (\o f -> f o) defaultOpts
    os = [ MA.OptSpec ["db"] "d"
           (MA.OneArg (\s o -> o { dbLocation = s }))

         , MA.OptSpec ["new"] "n"
           (MA.NoArg (\o -> o { allowNew = True }))

         , MA.OptSpec ["help"] "h"
           (MA.NoArg (\o -> o { showHelp = True })) ]



-- | Appends new Amex transactions to the existing list.
appendNew ::
  [(A.UNumber, A.AmexTxn)]
  -- ^ Existing transactions

  -> [A.AmexTxn]
  -- ^ New transactions

  -> ([(A.UNumber, A.AmexTxn)], Int)
  -- ^ New list, and number of transactions added

appendNew old new = (old ++ filteredNew, length filteredNew)
  where
    nextId = if null old then 0 else (maximum . map fst $ old) + 1
    set = Set.fromList . map A.amexId . map snd $ old
    newWithId = zip [nextId..] new
    filteredNew = filter (flip Set.notMember set . A.amexId . snd)
                  newWithId

main :: IO ()
main = do
  os <- fmap parseOpts MA.getArgs
  when (showHelp os) (putStr help >> exitSuccess)
  txnsOld <- A.loadDb (allowNew os) (dbLocation os)
  let path = case csvFile os of
        Nothing -> error "you must specify a file to import."
        Just p -> p
  ins <- A.loadIncoming path
  let (new, len) = appendNew txnsOld ins
  A.saveDb (dbLocation os) new
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

