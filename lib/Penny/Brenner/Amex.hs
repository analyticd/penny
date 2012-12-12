-- | Amex - processing American Express downloaded data
module Penny.Brenner.Amex
  ( Card(..)
  , Config(..)
  , R.GroupSpecs(..)
  , R.GroupSpec(..)
  , amexMain
  ) where

import qualified Penny.Brenner.Amex.Types as Y
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Render as R
import qualified Penny.Brenner.Amex.Clear as C
import qualified Penny.Brenner.Amex.Import as I
import qualified Penny.Brenner.Amex.Merge as M
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import System.Exit (exitFailure)
import qualified System.IO as IO

amexMain :: Config -> IO ()
amexMain cf = do
  as <- MA.getArgs
  pr <- MA.getProgName
  let cf' = convertConfig cf
      r = MA.modes globalOpts (preProcessor cf') whatMode as
  processParseResult pr cf' r

data Arg
  = AHelp
  | ACard String
  deriving (Eq, Show)

toCardOpt :: Arg -> Maybe String
toCardOpt a = case a of { ACard s -> Just s; _ -> Nothing }

globalOpts :: [MA.OptSpec Arg]
globalOpts =
  [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp)
  , MA.OptSpec ["card"] "c" (MA.OneArg ACard)
  ]

data PreProc
  = NeedsHelp
  | DoIt Y.Card

preProcessor :: Y.Config -> [Arg] -> Ex.Exceptional String PreProc
preProcessor cf as =
  if any (== AHelp) as
  then return NeedsHelp
  else do
    let cardOpt = case mapMaybe toCardOpt as of
          [] -> Nothing
          xs -> Just . last $ xs
    card <- case cardOpt of
      Nothing -> case Y.defaultCard cf of
        Nothing -> Ex.throw $ "no card given on command line, and no "
                   ++ "default card provided."
        Just c -> return c
      Just o ->
        let pdct (Y.Name n, _) = n == o
        in case filter pdct (Y.moreCards cf) of
          [] -> Ex.throw $ "card " ++ o ++ " not configured."
          (_, c):[] -> return c
          _ -> Ex.throw $ "more than one card named " ++ o
                          ++ " configured."
    return $ DoIt card

whatMode
  :: PreProc
  -> Either (String -> String)
      [MA.Mode String (Ex.Exceptional String (IO ()))]
whatMode pp = case pp of
  NeedsHelp -> Left id
  DoIt cd ->
    Right [ C.mode cd, I.mode (Y.dbLocation cd), M.mode cd ]


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
         , "-c, --card CARD"
         , "  Use one of the Additional Cards shown below. If this option"
         , "  does not appear, the default card is used if there"
         , "  is one."
         , "-h, --help"
         , "  Show help and exit"
         , ""
         ]
    showPair (Y.Name a, cd) = "Additional card: " ++ a
      ++ "\n" ++ showCard cd
    cs = showDefaultCard (Y.defaultCard c)
         ++ more
    more = if null (Y.moreCards c)
           then "No additional cards\n"
           else concatMap showPair . Y.moreCards $ c

showDefaultCard :: Maybe Y.Card -> String
showDefaultCard mc = case mc of
  Nothing -> "No default card\n"
  Just c -> "Default Card:\n" ++ showCard c

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

showCard :: Y.Card -> String
showCard c =
  label "Database location" (Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Amex ledger account"
     (showAccount . Y.unAmexAcct . Y.amexAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

  ++ "\n"

-- | Information to configure a single card account.
data Card = Card
  { dbLocation :: String
    -- ^ Path and filename to where the database is kept. You can use
    -- an absolute or relative path (if it is relative, it will be
    -- resolved relative to the current directory at runtime.)

  , amexAcct :: String
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

  } deriving Show

convertCard :: Card -> Y.Card
convertCard (Card db ax df cy gs) = Y.Card
  { Y.dbLocation = Y.DbLocation db
  , Y.amexAcct = Y.AmexAcct . Bd.account $ ax
  , Y.defaultAcct = Y.DefaultAcct . Bd.account $ df
  , Y.currency = Y.Currency . L.Commodity . X.pack $ cy
  , Y.groupSpecs = gs
  }

data Config = Config
  { defaultCard :: Maybe Card
  , moreCards :: [(String, Card)]
  } deriving Show

convertConfig :: Config -> Y.Config
convertConfig (Config d m) = Y.Config
  { Y.defaultCard = fmap convertCard d
  , Y.moreCards =
      let f (n, c) = (Y.Name n, convertCard c)
      in map f m
  }
