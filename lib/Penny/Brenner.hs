-- | Brenner - Penny financial institution interfaces
--
-- Brenner provides a uniform way to interact with downloaded data
-- from financial Given a parser, Brenner will import the transactions
-- and store them in a database. From there it is easy to merge the
-- transactions (without duplicates) into a ledger file, and then to
-- clear transactions from statements in an automated fashion.
module Penny.Brenner
  ( FitAcct(..)
  , Config(..)
  , R.GroupSpecs(..)
  , R.GroupSpec(..)
  , Y.Translator(..)
  , L.Side(..)
  , L.SpaceBetween(..)
  , usePayeeOrDesc
  , brennerMain
  , brennerDynamic
  ) where

import qualified Penny.Brenner.Types as Y
import Control.Monad (join)
import Data.Either (partitionEithers)
import Data.List (find)
import qualified Data.Text as X
import qualified Data.Version as V
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Steel.Sums as S
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Render as R
import qualified Penny.Brenner.Clear as C
import qualified Penny.Brenner.Database as D
import qualified Penny.Brenner.Import as I
import qualified Penny.Brenner.Merge as M
import qualified Penny.Brenner.OFX as O
import qualified Penny.Brenner.Print as P
import qualified Penny.Brenner.Util as U
import Control.Applicative ((<|>))
import qualified System.Console.MultiArg as MA
import System.Directory (getHomeDirectory)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | Brenner, with a pre-compiled configuration.
brennerMain
  :: V.Version
  -- ^ Binary version
  -> Config
  -> IO ()
brennerMain v cf = do
  let cf' = convertConfig cf
  join $ MA.modesWithHelp (help False) (globalOpts v)
                          (preProcessor cf')

-- | Brenner with a dynamic configuration.
brennerDynamic
  :: V.Version
  -- ^ Binary version
  -> IO ()
brennerDynamic v = do
  pn <- MA.getProgName
  as <- MA.getArgs
  case MA.modes (globalOptsDynamic v) preProcessorDynamic as of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr . MA.formatError pn $ e
      Exit.exitFailure
    Ex.Success g -> g

-- | Parses global options for a pre-compiled configuration.
globalOpts
  :: V.Version
  -- ^ Binary version
  -> [MA.OptSpec (Either (IO ()) Y.FitAcctName)]
globalOpts v =
  [ MA.OptSpec ["fit-account"] "f"
  (MA.OneArg (Right . Y.FitAcctName . X.pack))
  , fmap Left (Ly.version v)
  ]

-- | Parses global options for a dynamic configuration.
globalOptsDynamic
  :: V.Version
  -- ^ Binary version
  -> [MA.OptSpec (S.S3 (IO ())
                       Y.ConfigLocation
                       Y.FitAcctName)]
globalOptsDynamic v =
  [ fmap S.S3a (Ly.version v)

  , MA.OptSpec ["config-file"] "c"
    (MA.OneArg (S.S3b . Y.ConfigLocation . X.pack))

  , MA.OptSpec ["fit-account"] "f"
    (MA.OneArg (S.S3c . Y.FitAcctName . X.pack))

  ]

-- | Pre-processes global options for a pre-compiled configuration.
preProcessor
  :: Y.Config
  -> [Either (IO ()) Y.FitAcctName]
  -> Either (a -> IO ()) [MA.Mode (IO ())]
preProcessor cf args =
  let (vers, as) = partitionEithers args
  in case vers of
      [] -> makeModes cf as
      x:_ -> Left (const x)

-- | Pre-processes global options for a dynamic configuration.
preProcessorDynamic
  :: [S.S3 (IO ()) Y.ConfigLocation Y.FitAcctName]
  -> Either (a -> IO ()) [MA.Mode (IO ())]
preProcessorDynamic ls =
  let (vs, cs, fs) = S.partitionS3 ls
      mkAct = applyAcctInMode cs fs
  in case vs of
      [] -> Right $ map (fmap mkAct) allModes
      x:_ -> Left (const x)


getDynamicConfig :: Y.ConfigLocation -> IO Y.Config
getDynamicConfig (Y.ConfigLocation s) =
  O.parseOFXConfigFile . X.unpack $ s

getConfigLocation
  :: [Y.ConfigLocation]
  -> IO Y.ConfigLocation
getConfigLocation cs =
  case cs of
    [] -> do
      pn <- MA.getProgName
      home <- getHomeDirectory
      return . Y.ConfigLocation . X.pack
        $ home ++ "/." ++ pn ++ ".ini"
    xs -> return $ last xs

applyAcctInMode
  :: [Y.ConfigLocation]
  -> [Y.FitAcctName]
  -> (Y.FitAcct -> IO ())
  -> IO ()
applyAcctInMode lsc lsf act = do
  configLoc <- getConfigLocation lsc
  conf <- getDynamicConfig configLoc
  acct <- getDynamicDefaultFitAcct lsf conf
  act acct

getDynamicDefaultFitAcct
  :: [Y.FitAcctName]
  -> Y.Config
  -> IO Y.FitAcct
getDynamicDefaultFitAcct cs c =
  case cs of
    [] -> case Y.defaultFitAcct c of
      Nothing -> errExit $ "no financial institution account"
        ++ " selected on command line, and no default"
        ++ " financial instititution account configured."
      Just a -> return a
    fs ->
      let name = last fs
          pd a = Y.fitAcctName a == name
          confDflt = maybe Nothing
            (\a -> if pd a then Just a else Nothing)
            $ Y.defaultFitAcct c
          confExtra = find pd . Y.moreFitAccts $ c
      in case confDflt <|> confExtra of
          Nothing -> errExit $ "financial institution account not "
            ++ "configured: " ++ (X.unpack . Y.unFitAcctName $ name)
          Just a -> return a

errExit :: String -> IO a
errExit s = do
  pn <- MA.getProgName
  IO.hPutStrLn IO.stderr $ pn ++ ": error: " ++ s
  Exit.exitFailure

-- | Makes modes for a pre-compiled configuration.
makeModes
  :: Y.Config
  -> [Y.FitAcctName]
  -- ^ Names of financial institutions given on command line
  -> Either (a -> IO ()) [MA.Mode (IO ())]
makeModes cf as = Ex.toEither . Ex.mapException (const . errExit) $ do
  fi <- case as of
    [] -> case Y.defaultFitAcct cf of
      Nothing -> Ex.throw $ "no financial institution account"
        ++ " selected on command line, and no default"
        ++ " financial instititution account configured."
      Just a -> return a
    _ ->
      let pdct a = Y.fitAcctName a == s
          s = last as
      in case filter pdct (Y.moreFitAccts cf) of
           [] -> Ex.throw $
              "financial institution account "
              ++ (X.unpack . Y.unFitAcctName $ s) ++ " not configured."
           c:[] -> return c
           _ -> Ex.throw $
              "more than one financial institution account "
              ++ "named " ++ (X.unpack . Y.unFitAcctName $ s)
              ++ " configured."
  return . map (fmap ($ fi)) $ allModes

allModes :: [MA.Mode (Y.FitAcct -> IO ())]
allModes = [C.mode, I.mode, M.mode, P.mode, D.mode]

-- | Help for a pre-compiled configuration.
help
  :: Bool
  -- ^ True if running under a dynamic configuration
  -> String
  -- ^ Program name

  -> String
help dyn n = unlines ls
  where
    ls = [ "usage: " ++ n ++ " [global-options]"
            ++ " COMMAND [local-options]"
            ++ " ARGS..."
         , ""
         , "where COMMAND is one of:"
         , "import, merge, clear, database, print, info"
         , ""
         , "For help on an individual command and its"
           ++ " local options, use "
         , n ++ " COMMAND --help"
         , ""
         , "Global Options:"
         , "-f, --fit-account ACCOUNT"
         , "  Use one of the Additional Financial Institution"
         , "  Accounts shown below. If this option does not appear,"
         , "  the default account is used if there is one."
         ] ++ if dyn then [] else
                  [ "-c, --config-file FILENAME"
                  , "  Specify configuration file location"
                  ]

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

-- | Information to configure a single financial institution account.
data FitAcct = FitAcct
  { fitAcctName :: String
    -- ^ Name for this financial institution account, e.g. @House
    -- Checking@ or @Megabank@.

  , fitAcctDesc :: String
    -- ^ Additional information about this financial institution
    -- account. Here I put information on where to find the statments
    -- for download on the website.

  , dbLocation :: String
    -- ^ Path and filename to where the database is kept. You can use
    -- an absolute or relative path (if it is relative, it will be
    -- resolved relative to the current directory at runtime.)

  , pennyAcct :: String
    -- ^ The account that you use in your Penny file to hold
    -- transactions for this card. Separate each sub-account with
    -- colons (as you do in the Penny file.)

  , defaultAcct :: String
    -- ^ When new transactions are created, one of the postings will
    -- be in the amexAcct given above. The other posting will be in
    -- this account.

  , currency :: String
    -- ^ The commodity for the currency of your card (e.g. @$@).

  , groupSpecs :: R.GroupSpecs
    -- ^ How to group digits when printing the resulting ledger. All
    -- quantities (not just those affected by this program) will be
    -- formatted using this specification.

  , translator :: Y.Translator
    -- ^ See the documentation under the 'Translator' type for
    -- details.

  , side :: L.Side
  -- ^ When creating new transactions, the commodity will be on this
  -- side

  , spaceBetween :: L.SpaceBetween
  -- ^ When creating new transactions, is there a space between the
  -- commodity and the quantity

  , parser :: ( Y.ParserDesc
              , Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -- ^ Parses a file of transactions from the financial
  -- institution. The function must open the file and parse it. This
  -- is in the IO monad not only because the function must open the
  -- file itself, but also so the function can perform arbitrary IO
  -- (run pdftotext, maybe?) If there is failure, the function can
  -- return an Exceptional String, which is the error
  -- message. Alternatively the function can raise an exception in the
  -- IO monad (currently Brenner makes no attempt to catch these) so
  -- if any of the IO functions throw you can simply not handle the
  -- exceptions.
  --
  -- The first element of the pair is a help string which should
  -- indicate how to download the data, as a helpful reminder.

  , toLincolnPayee :: Y.Desc -> Y.Payee -> L.Payee
  -- ^ Sometimes the financial institution provides Payee information,
  -- sometimes it does not. Sometimes the Desc might have additional
  -- information that you might want to remove. This function can be
  -- used to do that. The resulting Lincoln Payee is used for any
  -- transactions that are created by the merge command. The resulting
  -- payee is also used when comparing new financial institution
  -- postings to already existing ledger transactions in order to
  -- guess at which payee and accounts to create in the transactions
  -- created by the merge command.


  } deriving Show

convertFitAcct :: FitAcct -> Y.FitAcct
convertFitAcct (FitAcct fn fd db ax df cy gs tl sd sb ps tlp) = Y.FitAcct
  { Y.fitAcctName = Y.FitAcctName . X.pack $ fn
  , Y.fitAcctDesc = Y.FitAcctDesc . X.pack $ fd
  , Y.dbLocation = Y.DbLocation . X.pack $ db
  , Y.pennyAcct = Y.PennyAcct . Bd.account . X.pack $ ax
  , Y.defaultAcct = Y.DefaultAcct . Bd.account . X.pack $ df
  , Y.currency = Y.Currency . L.Commodity . X.pack $ cy
  , Y.groupSpecs = gs
  , Y.translator = tl
  , Y.side = sd
  , Y.spaceBetween = sb
  , Y.parser = ps
  , Y.toLincolnPayee = tlp
  }

data Config = Config
  { defaultFitAcct :: Maybe FitAcct
  , moreFitAccts :: [FitAcct]
  } deriving Show

convertConfig :: Config -> Y.Config
convertConfig (Config d m) = Y.Config
  { Y.defaultFitAcct = fmap convertFitAcct d
  , Y.moreFitAccts = map convertFitAcct m
  }

-- | A simple function to use for 'toLincolnPayee'. Uses the financial
-- institution payee if it is available; otherwise, uses the financial
-- institution description.
usePayeeOrDesc :: Y.Desc -> Y.Payee -> L.Payee
usePayeeOrDesc (Y.Desc d) (Y.Payee p) = L.Payee $
  if X.null p then d else p
