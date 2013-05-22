module Penny.Brenner.Import (mode) where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)
import qualified System.Console.MultiArg as MA
import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U

data Arg
  = AFitFile String
  | AAllowNew
  | AUNumber Integer

toFitFile :: Arg -> Maybe String
toFitFile a = case a of
  AFitFile s -> Just s
  _ -> Nothing

toNewUNumber :: [Arg] -> Maybe Integer
toNewUNumber as =
  let f i = case i of { AUNumber x -> Just x; _ -> Nothing }
  in case mapMaybe f as of
      [] -> Nothing
      xs -> Just $ last xs

data ImportOpts = ImportOpts
  { fitFile :: Y.FitFileLocation
  , allowNew :: Y.AllowNew
  , parser :: Y.FitFileLocation
              -> IO (Ex.Exceptional String [Y.Posting])
  , newUNumber :: Maybe Integer
  }

mode
  :: MA.Mode (Maybe Y.FitAcct -> IO ())
mode = MA.Mode
  { MA.mName = "import"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts =
      [ MA.OptSpec ["new"] "n" (MA.NoArg AAllowNew)
      , MA.OptSpec ["unumber"] "u" . MA.OneArgE $ \s -> do
          i <- MA.reader s
          return $ AUNumber i
      ]
  , MA.mPosArgs = return . AFitFile
  , MA.mProcess = processor
  , MA.mHelp = help
  }

processor
  :: [Arg]
  -> Maybe Y.FitAcct
  -> IO ()
processor as mayFa = do
  fa <- U.getFitAcct mayFa
  let (dbLoc, prsr) = (Y.dbLocation fa, snd . Y.parser $ fa)
  loc <- case mapMaybe toFitFile as of
    [] -> fail "you must provide a postings file to read"
    x:[] -> return (Y.FitFileLocation x)
    _ -> fail "you cannot provide more than one postings file to read"
  let aNew = Y.AllowNew
        $ any (\a -> case a of { AAllowNew -> True; _ -> False }) as
      maybeNewU = toNewUNumber as
  doImport dbLoc (ImportOpts loc aNew prsr maybeNewU)


-- | Appends new Amex transactions to the existing list.
appendNew
  :: Maybe Integer
  -- ^ If Just, this is the new U-number for the first
  -- transaction. Otherwise, the next U number will be the one that is
  -- one larger than the current maximum in the database.

  -> [(Y.UNumber, Y.Posting)]
  -- ^ Existing transactions

  -> [Y.Posting]
  -- ^ New transactions

  -> Maybe ([(Y.UNumber, Y.Posting)], Int)
  -- ^ New list, and number of transactions added. Fails if the new U
  -- number was passed in the first argument and this number is not
  -- valid.

appendNew mu db new =
  let currFitIds = map (Y.fitId . snd) db
      isNew p = not (any (== Y.fitId p) currFitIds)
      newPstgs = filter isNew new
      mkPair i p = (Y.UNumber i, p)
      maybeU = nextUNum mu db
  in fmap (\u -> let newWithU = (zipWith mkPair [u..] newPstgs)
                 in (db ++ newWithU, length newWithU)) maybeU

nextUNum
  :: Maybe Integer
  -> [(Y.UNumber, Y.Posting)]
  -> Maybe Integer
nextUNum mu db =
  let defaultU = if null db then Nothing
                 else Just $ ( Y.unUNumber . maximum
                               . map fst $ db) + 1
  in case mu of
      Nothing -> defaultU <|> Just 0
      Just u -> case defaultU of
        Nothing -> if u >= 0 then Just u else Nothing
        Just du -> if u >= du then Just u else Nothing

doImport :: Y.DbLocation -> ImportOpts -> IO ()
doImport dbLoc os = do
  txnsOld <- U.loadDb (allowNew os) dbLoc
  parseResult <- parser os (fitFile os)
  ins <- case parseResult of
    Ex.Exception e -> fail e
    Ex.Success g -> return g
  (new, len) <- case appendNew (newUNumber os) txnsOld ins of
    Just r -> return r
    Nothing -> fail "invalid new U number given."
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
  , "-u, --unumber - The first U number assigned will be this number."
  , "Fails if the number you give is not greater than the largest"
  , "U number already in the database."
  , ""
  , "-h, --help - Show this help."
  , ""
  ]

