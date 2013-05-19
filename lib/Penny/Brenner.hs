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
  ) where

import qualified Penny.Brenner.Types as Y
import Control.Monad (join)
import Data.Either (partitionEithers)
import qualified Data.Text as X
import qualified Data.Version as V
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Render as R
import qualified Penny.Brenner.Clear as C
import qualified Penny.Brenner.Database as D
import qualified Penny.Brenner.Import as I
import qualified Penny.Brenner.Merge as M
import qualified Penny.Brenner.Print as P
import qualified Penny.Brenner.Util as U
import Control.Applicative ((<*>))
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.Exit as Exit
import qualified System.IO as IO

brennerMain
  :: V.Version
  -- ^ Binary version
  -> Config
  -> IO ()
brennerMain v cf = do
  let cf' = convertConfig cf
  pn <- MA.getProgName
  as <- MA.getArgs
  case MA.modes (globalOpts cf' v) (preProcessor cf') as of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr . MA.formatError pn $ e
      Exit.exitFailure
    Ex.Success g -> g

-- | Parses global options for a pre-compiled configuration.
globalOpts
  :: Y.Config
  -> V.Version
  -- ^ Binary version
  -> [MA.OptSpec (Either (IO ()) FitAcctName)]
globalOpts cf v =
  [ MA.OptSpec ["fit-account"] "f"
  (MA.OneArg (Right . FitAcctName . Y.Name . X.pack))
  , fmap Left (Ly.version v)

  , fmap Left $ U.help (help cf)
  ]

newtype ConfigLocation = ConfigLocation
  { unConfigLocation :: String }
  deriving (Eq, Show)

newtype FitAcctName = FitAcctName
  { unFitAcctName :: Y.Name }
  deriving (Eq, Show)

-- | Parses global options for a dynamic configuration.
globalOptsDynamic
  :: V.Version
  -- ^ Binary version
  -> [MA.OptSpec (Either (IO ()) (Either ConfigLocation FitAcctName)) ]
globalOptsDynamic v =
  [ fmap Left (Ly.version v)

  , MA.OptSpec ["config-file"] "c"
    (MA.OneArg (Right . Left . ConfigLocation))

  , MA.OptSpec ["fit-account"] "f"
    (MA.OneArg (Right . Right . FitAcctName . Y.Name . X.pack))
  ]

-- | Pre-processes global options for a pre-compiled configuration.
preProcessor
  :: Y.Config
  -> [Either (IO ()) FitAcctName]
  -> Either (a -> IO ()) [MA.Mode (IO ())]
preProcessor cf args =
  let (vers, as) = partitionEithers args
  in case vers of
      [] -> makeModes cf as
      x:_ -> Left (const x)

-- | Pre-processes global options for a dynamic configuration.
preProcessorDynamic
  :: [Either (IO ()) (Either ConfigLocation FitAcctName)]
  -> Either (a -> IO ()) [MA.Mode (IO ())]
preProcessorDynamic args =
  let (versOrHelp, as) = partitionEithers args
  in case versOrHelp of
    x:_ -> Left (const x)
    [] -> makeModesDynamic as

makeModesDynamic
  :: [Either ConfigLocation FitAcctName]
  -> Either (a -> IO ()) [MA.Mode (IO ())]
makeModesDynamic = undefined

-- | Makes modes for a pre-compiled configuration.
makeModes
  :: Y.Config
  -> [FitAcctName]
  -- ^ Names of financial institutions given on command line
  -> Either (a -> IO ()) [MA.Mode (IO ())]
makeModes cf as = Ex.toEither . Ex.mapException (const . fail) $ do
  fi <- case as of
    [] -> case Y.defaultFitAcct cf of
      Nothing -> fail $ "no financial institution account"
        ++ " selected on command line, and no default"
        ++ " financial instititution account configured."
      Just a -> return a
    _ ->
      let pdct (Y.Name n, _) = n == s
          FitAcctName (Y.Name s) = last as
      in case filter pdct (Y.moreFitAccts cf) of
           [] -> Ex.throw $
              "financial institution account "
              ++ X.unpack s ++ " not configured."
           (_, c):[] -> return c
           _ -> Ex.throw $
              "more than one financial institution account "
              ++ "named " ++ X.unpack s ++ " configured."
  let ms = [C.mode, I.mode, M.mode, P.mode, D.mode]
  return . map (fmap ($ fi)) $ ms

-- | Help for a pre-compiled configuration.
help
  :: Y.Config
  -> String
  -- ^ Program name

  -> String
help c n = unlines ls ++ cs
  where
    ls = [ "usage: " ++ n ++ " [global-options]"
            ++ " COMMAND [local-options]"
            ++ " ARGS..."
         , ""
         , "where COMMAND is one of:"
         , "import, merge, clear, database, print"
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
         , "-h, --help"
         , "  Show help and exit"
         , ""
         ]
    showPair (Y.Name a, cd) = "Additional financial institution "
      ++ "account: " ++ X.unpack a ++ "\n" ++ showFitAcct cd
    cs = showDefaultFitAcct (Y.defaultFitAcct c)
         ++ more
    more = if null (Y.moreFitAccts c)
           then "No additional financial institution accounts\n"
           else concatMap showPair . Y.moreFitAccts $ c

showDefaultFitAcct :: Maybe Y.FitAcct -> String
showDefaultFitAcct mc = case mc of
  Nothing -> "No default financial institution account\n"
  Just c -> "Default financial institution account:\n" ++ showFitAcct c

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

showFitAcct :: Y.FitAcct -> String
showFitAcct c =
  label "Database location"
    (X.unpack . Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Penny account"
     (showAccount . Y.unPennyAcct . Y.pennyAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

  ++ "\n"

  ++ "More information about the parser:\n"
  ++ (fst . Y.parser $ c)
  ++ "\n\n"


-- | Information to configure a single financial institution account.
data FitAcct = FitAcct
  { dbLocation :: String
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

  , parser :: ( String
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
convertFitAcct (FitAcct db ax df cy gs tl sd sb ps tlp) = Y.FitAcct
  { Y.dbLocation = Y.DbLocation . X.pack $ db
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
  , moreFitAccts :: [(String, FitAcct)]
  } deriving Show

convertConfig :: Config -> Y.Config
convertConfig (Config d m) = Y.Config
  { Y.defaultFitAcct = fmap convertFitAcct d
  , Y.moreFitAccts =
      let f (n, c) = (Y.Name (X.pack n), convertFitAcct c)
      in map f m
  }

-- | A simple function to use for 'toLincolnPayee'. Uses the financial
-- institution payee if it is available; otherwise, uses the financial
-- institution description.
usePayeeOrDesc :: Y.Desc -> Y.Payee -> L.Payee
usePayeeOrDesc (Y.Desc d) (Y.Payee p) = L.Payee $
  if X.null p then d else p
