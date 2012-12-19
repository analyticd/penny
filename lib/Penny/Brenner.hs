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
  , brennerMain
  ) where

import qualified Penny.Brenner.Types as Y
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Render as R
import qualified Penny.Brenner.Clear as C
import qualified Penny.Brenner.Database as D
import qualified Penny.Brenner.Import as I
import qualified Penny.Brenner.Merge as M
import qualified Penny.Brenner.Print as P
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import System.Exit (exitFailure)
import qualified System.IO as IO

brennerMain :: Config -> IO ()
brennerMain cf = do
  as <- MA.getArgs
  pr <- MA.getProgName
  let cf' = convertConfig cf
      r = MA.modes globalOpts (preProcessor cf') whatMode as
  processParseResult pr cf' r

data Arg
  = AHelp
  | AFitAcct String
  deriving (Eq, Show)

toFitAcctOpt :: Arg -> Maybe String
toFitAcctOpt a = case a of { AFitAcct s -> Just s; _ -> Nothing }

globalOpts :: [MA.OptSpec Arg]
globalOpts =
  [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp)
  , MA.OptSpec ["fit-account"] "f" (MA.OneArg AFitAcct)
  ]

data PreProc
  = NeedsHelp
  | DoIt Y.FitAcct

preProcessor :: Y.Config -> [Arg] -> Ex.Exceptional String PreProc
preProcessor cf as =
  if any (== AHelp) as
  then return NeedsHelp
  else do
    let cardOpt = case mapMaybe toFitAcctOpt as of
          [] -> Nothing
          xs -> Just . last $ xs
    card <- case cardOpt of
      Nothing -> case Y.defaultFitAcct cf of
        Nothing -> Ex.throw $ "no financial institution account "
                              ++ "given on command line, and no "
                              ++ "default card provided."
        Just c -> return c
      Just o ->
        let pdct (Y.Name n, _) = n == X.pack o
        in case filter pdct (Y.moreFitAccts cf) of
          [] -> Ex.throw $ "financial institution account "
                           ++ o ++ " not configured."
          (_, c):[] -> return c
          _ -> Ex.throw $ "more than one financial institution account "
                          ++ "named " ++ o ++ " configured."
    return $ DoIt card

whatMode
  :: PreProc
  -> Either (String -> String)
      [MA.Mode String (Ex.Exceptional String (IO ()))]
whatMode pp = case pp of
  NeedsHelp -> Left id
  DoIt cd ->
    Right [ C.mode cd
          , I.mode (Y.dbLocation cd) (snd . Y.parser $ cd)
          , M.mode cd
          , P.mode (snd . Y.parser $ cd)
          , D.mode (Y.dbLocation cd)
          ]


processParseResult
  :: String
  -- ^ Program name
  -> Y.Config

  -> Ex.Exceptional MA.Error
      (a, Either b (c, Ex.Exceptional String (IO ())))
  -> IO ()
processParseResult pr cf ex =
  case ex of
    Ex.Exception err -> do
      IO.hPutStr IO.stderr $ MA.formatError pr err
      exitFailure
    Ex.Success g -> processResult pr cf g


processResult
  :: String
  -- ^ Program name

  -> Y.Config
  -> (a, Either b (c, Ex.Exceptional String (IO ())))
  -> IO ()
processResult pr cf (_, ei) =
  case ei of
    Left _ -> putStr (help pr cf)
    Right (_, ex) -> case ex of
      Ex.Exception e -> do
        putStrLn $ pr ++ ": error: " ++ e
        exitFailure
      Ex.Success g -> g

help ::
  String
  -- ^ Program name

  -> Y.Config
  -> String
help n c = unlines ls ++ cs
  where
    ls = [ "usage: " ++ n ++ " [options] import|merge|clear ARGS..."
         , ""
         , "For help on an individual command, use "
         , n ++ " COMMAND --help"
         , ""
         , "Options:"
         , "-f, --fit-account CARD"
         , "  Use one of the Additional Financial Institution"
         , "  Accounts shown below. If this option does not appear,"
         , "  the default card is used if there is one."
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

  ++ "More information about the parser:\n"
  ++ (fst . Y.parser $ c)


-- | Information to configure a single card account.
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


  } deriving Show

convertFitAcct :: FitAcct -> Y.FitAcct
convertFitAcct (FitAcct db ax df cy gs tl sd sb ps) = Y.FitAcct
  { Y.dbLocation = Y.DbLocation . X.pack $ db
  , Y.pennyAcct = Y.PennyAcct . Bd.account $ ax
  , Y.defaultAcct = Y.DefaultAcct . Bd.account $ df
  , Y.currency = Y.Currency . L.Commodity . X.pack $ cy
  , Y.groupSpecs = gs
  , Y.translator = tl
  , Y.side = sd
  , Y.spaceBetween = sb
  , Y.parser = ps
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
