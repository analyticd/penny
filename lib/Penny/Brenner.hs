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
  , Su.S3(..)
  , L.Radix(..)
  , L.PeriodGrp(..)
  , L.CommaGrp(..)
  , Y.Translator(..)
  , L.Side(..)
  , L.SpaceBetween(..)
  , usePayeeOrDesc
  , brennerMain
  , ofxParser
  ) where

import qualified Penny.Brenner.Types as Y
import qualified Data.Text as X
import qualified Data.Version as V
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Brenner.Clear as C
import qualified Penny.Brenner.Database as D
import qualified Penny.Brenner.Import as I
import qualified Penny.Brenner.Info as Info
import qualified Penny.Brenner.Merge as M
import qualified Penny.Brenner.OFX as O
import qualified Penny.Brenner.Print as P
import qualified Penny.Steel.Sums as Su
import qualified System.Console.MultiArg as MA
import System.Environment (getProgName)
import qualified System.Exit as Exit
import qualified Control.Monad.Exception.Synchronous as Ex

-- | Brenner, with a pre-compiled configuration.
brennerMain
  :: V.Version
  -- ^ Binary version
  -> Config
  -> IO ()
brennerMain v cf = do
  let cf' = convertConfig cf
  pn <- getProgName
  ei <- MA.modesIO (globalOpts v) (preProcessor cf')
  case ei of
    Left showHelp -> putStr (showHelp pn) >> Exit.exitSuccess
    Right g -> g

type GetHelp = MA.ProgName -> String

-- | Parses global options for a pre-compiled configuration.
globalOpts
  :: V.Version
  -- ^ Binary version
  -> MA.Opts GetHelp Y.FitAcctName
globalOpts v = MA.optsHelpVersion help (Ly.version v)
  [ MA.OptSpec ["fit-account"] "f"
               (MA.OneArg (Y.FitAcctName . X.pack))
  ]

-- | Pre-processes global options for a pre-compiled configuration.
preProcessor
  :: Y.Config
  -> [Y.FitAcctName]
  -> Ex.Exceptional String
      (Either (IO ())
              [MA.Mode (MA.ProgName -> String) (IO ())])
preProcessor cf args = do
  mayFi <- case args of
    [] -> return $ Y.defaultFitAcct cf
    _ ->
      let pdct a = Y.fitAcctName a == s
          s = last args
          toFilter = case Y.defaultFitAcct cf of
            Nothing -> Y.moreFitAccts cf
            Just d -> d : Y.moreFitAccts cf
      in case filter pdct toFilter of
           [] -> Ex.throw $
              "financial institution account "
              ++ (X.unpack . Y.unFitAcctName $ s) ++ " not configured."
           c:[] -> return $ Just c
           _ -> Ex.throw $
              "more than one financial institution account "
              ++ "named " ++ (X.unpack . Y.unFitAcctName $ s)
              ++ " configured."
  return . Right $ allModes cf mayFi

-- | Each mode takes a Maybe FitAcct. Even if every mode needs a
-- FitAcct to function, they take a Maybe FitAcct because otherwise
-- the user will not even get online help if a FitAcct is not
-- supplied. Each mode must fail on its own if it actually needs a
-- FitAcct.

allModes
  :: Y.Config
  -> Maybe Y.FitAcct
  -> [MA.Mode (MA.ProgName -> String) (IO ())]
allModes c a =
  [ C.mode a
  , I.mode a
  , Info.mode c
  , M.mode a
  , P.mode a
  , D.mode a ]

-- | Help for a pre-compiled configuration.
help
  :: String
  -- ^ Program name

  -> String
help n = unlines ls
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
         , "  use the given financial institution account"
         , "  (use the \"info\" command to see which are available)."
         , "  If this option does not appear,"
         , "  the default account is used if there is one."
         ]

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

  , qtySpec :: Su.S3 L.Radix L.PeriodGrp L.CommaGrp
    -- ^ How to group digits when printing the resulting ledger.
    --
    -- Penny remembers the formatting of quantities entered in your
    -- ledger.  However, quantities imported from your bank statement
    -- do not have formatting to remember, so you have to tell Penny
    -- how to format them.

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


  }

convertFitAcct :: FitAcct -> Y.FitAcct
convertFitAcct (FitAcct fn fd db ax df cy gs tl sd sb ps tlp) = Y.FitAcct
  { Y.fitAcctName = Y.FitAcctName . X.pack $ fn
  , Y.fitAcctDesc = Y.FitAcctDesc . X.pack $ fd
  , Y.dbLocation = Y.DbLocation . X.pack $ db
  , Y.pennyAcct = Y.PennyAcct . Bd.account . X.pack $ ax
  , Y.defaultAcct = Y.DefaultAcct . Bd.account . X.pack $ df
  , Y.currency = Y.Currency . L.Commodity . X.pack $ cy
  , Y.qtySpec = gs
  , Y.translator = tl
  , Y.side = sd
  , Y.spaceBetween = sb
  , Y.parser = ps
  , Y.toLincolnPayee = tlp
  }

data Config = Config
  { defaultFitAcct :: Maybe FitAcct
  , moreFitAccts :: [FitAcct]
  }

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

-- | Parser for OFX data.
ofxParser :: (Y.ParserDesc, Y.ParserFn)
ofxParser = O.parser
