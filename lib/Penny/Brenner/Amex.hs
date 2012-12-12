-- | Amex - processing American Express downloaded data
module Penny.Brenner.Amex where

import qualified Penny.Brenner.Amex.Types as Y
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Penny.Lincoln as L
import qualified Penny.Brenner.Amex.Clear as C
import qualified Penny.Brenner.Amex.Import as I
import qualified Penny.Brenner.Amex.Merge as M
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import System.Exit (exitFailure)
import qualified System.IO as IO

amexMain :: Y.Config -> IO ()
amexMain cf = do
  as <- MA.getArgs
  pr <- MA.getProgName
  let r = MA.modes globalOpts (preProcessor cf) whatMode as
  processParseResult pr cf r

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
      IO.hPutStrLn IO.stderr . show $ err
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
    Left _ -> putStrLn (help pr cf)
    Right (_, ex) -> case ex of
      Ex.Exception e ->
        putStrLn $ pr ++ ": error: " ++ e
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
         , "For help on an individual command, use"
         , n ++ "COMMAND --help"
         , "Options:"
         , "-c, --card CARD"
         , "  Use one of the cards shown below. If this option"
         , "  does not appear, the default card is used if there"
         , "  is one."
         , "-h, --help"
         , "  Show help and exit"
         ]
    showPair (Y.Name a, cd) = showCard a cd
    cs = showDefaultCard (Y.defaultCard c)
         ++ (concatMap showPair . Y.moreCards $ c)

showDefaultCard :: Maybe Y.Card -> String
showDefaultCard mc = case mc of
  Nothing -> "No default card\n"
  Just c -> showCard "<DEFAULT>" c

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

showCard :: String -> Y.Card -> String
showCard n c =
  "Card " ++ n ++ ":\n"
  ++ label "Database location" (Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Amex ledger account"
     (showAccount . Y.unAmexAcct . Y.amexAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

