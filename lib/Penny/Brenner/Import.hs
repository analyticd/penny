module Penny.Brenner.Import (mode) where

import Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)
import qualified System.Console.MultiArg as MA
import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U


data Arg
  = AFitFile String
  | AAllowNew
  deriving (Eq, Show)

toFitFile :: Arg -> Maybe String
toFitFile a = case a of
  AFitFile s -> Just s
  _ -> Nothing

data ImportOpts = ImportOpts
  { fitFile :: Y.FitFileLocation
  , allowNew :: Y.AllowNew
  , parser :: Y.FitFileLocation
              -> IO (Ex.Exceptional String [Y.Posting])
  }

mode
  :: Maybe Y.FitAcct
  -> MA.Mode (IO ())
mode mayFa = MA.Mode
  { MA.mName = "import"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts =
      [ MA.OptSpec ["new"] "n" (MA.NoArg AAllowNew) ]
  , MA.mPosArgs = return . AFitFile
  , MA.mProcess = processor mayFa
  , MA.mHelp = help
  }

processor
  :: Maybe Y.FitAcct
  -> [Arg]
  -> IO ()
processor mayFa as = do
  (dbLoc, prsr) <- case mayFa of
    Nothing -> fail $ "no financial institution account provided"
      ++ " on command line, and no default financial institution"
      ++ " account is configured."
    Just fa -> return (Y.dbLocation fa, snd . Y.parser $ fa)
  loc <- case mapMaybe toFitFile as of
    [] -> fail "you must provide a postings file to read"
    x:[] -> return (Y.FitFileLocation x)
    _ -> fail "you cannot provide more than one postings file to read"
  let aNew = Y.AllowNew $ any (== AAllowNew) as
  doImport dbLoc (ImportOpts loc aNew prsr)


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
    currFitIds = map (Y.fitId . snd) db
    isNew p = not (any (== Y.fitId p) currFitIds)
    newPstgs = filter isNew new
    mkPair i p = (Y.UNumber i, p)
    newWithU = zipWith mkPair [nextUNum..] newPstgs


doImport :: Y.DbLocation -> ImportOpts -> IO ()
doImport dbLoc os = do
  txnsOld <- U.loadDb (allowNew os) dbLoc
  parseResult <- parser os (fitFile os)
  ins <- case parseResult of
    Ex.Exception e -> fail e
    Ex.Success g -> return g
  let (new, len) = appendNew txnsOld ins
  U.saveDb dbLoc new
  putStrLn $ "imported " ++ show len ++ " new transactions."

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ "  [global-options] import [local-options] FIT_FILE"
  , "where FIT_FILE is the file downloaded from the financial"
  , "institution."
  , ""
  , "Local Options:"
  , ""
  , "-n, --new - Allows creation of new database. Without this option,"
  , "if the database file is not found, quits with an error."
  , ""
  , "-h, --help - Show this help."
  , ""
  ]

