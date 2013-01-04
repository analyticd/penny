-- | Zinc - the Penny command-line interface
module Penny.Zinc
  ( Defaults(..)
  , ColorToFile(..)
  , defaultFromRuntime
  , runPenny
  ) where

import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Scheme.Dark as Dark
import qualified Penny.Cabin.Scheme.Light as Light
import qualified Penny.Copper as C
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as X
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (when)
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid (mappend, mconcat)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified System.Console.MultiArg as MA
import System.Console.MultiArg.GetArgs (getArgs)
import System.Exit (exitSuccess, exitFailure)
import qualified System.IO as IO
import System.IO (hIsTerminalDevice, stdin, stderr, hPutStrLn)
import qualified Text.Matchers.Text as M

runPenny
  :: (S.Runtime -> Defaults)
  -> [I.Report]
  -> IO ()
runPenny df rs = do
  rt <- S.runtime
  as <- getArgs
  parseAndPrint (df rt) rs rt as

data Defaults = Defaults
  { sensitive :: M.CaseSensitive
  , factory :: M.CaseSensitive -> Text
               -> Ex.Exceptional Text (Text -> Bool)
  , currentTime :: L.DateTime
  , colorToFile :: ColorToFile
  , scheme :: E.Scheme
  }

defaultFromRuntime
  :: S.Runtime
  -> Defaults
defaultFromRuntime rt = Defaults
  { sensitive = M.Insensitive
  , factory = (\c t -> return (M.within c t))
  , currentTime = S.currentTime rt
  , colorToFile = ColorToFile False
  , scheme = Dark.scheme
  }

-- | Whether to use color when standard output is not a terminal.
newtype ColorToFile = ColorToFile { unColorToFile :: Bool }
  deriving (Eq, Show)

--
-- Option parsing
--

--
-- OptResult, and functions dealing with it
--
data OptResult
  = ROperand (M.CaseSensitive
             -> Ly.MatcherFactory
             -> Ex.Exceptional String Ly.Operand)
  | RPostFilter (Ex.Exceptional String Ly.PostFilterFn)
  | RMatcherSelect Ly.MatcherFactory
  | RCaseSelect M.CaseSensitive
  | ROperator (Ly.Token (L.PostFam -> Bool))
  | RSortSpec (Ex.Exceptional String Ly.Orderer)
  | RHelp
  | RColorToFile ColorToFile
  | RScheme E.Scheme

isHelp :: OptResult -> Bool
isHelp o = case o of { RHelp -> True; _ -> False }

getPostFilters
  :: [OptResult]
  -> Ex.Exceptional String [Ly.PostFilterFn]
getPostFilters =
  sequence
  . mapMaybe f
  where
    f o = case o of
      RPostFilter pf -> Just pf
      _ -> Nothing

getSortSpec
  :: [OptResult]
  -> Ex.Exceptional String Ly.Orderer
getSortSpec =
  fmap mconcat
  . sequence
  . mapMaybe f
  where
    f o = case o of
      RSortSpec x -> Just x
      _ -> Nothing

type Factory = M.CaseSensitive
             -> Text -> Ex.Exceptional Text (Text -> Bool)

makeToken
  :: OptResult
  -> St.State (M.CaseSensitive, Factory)
              (Maybe (Ex.Exceptional String (Ly.Token (L.PostFam -> Bool))))
makeToken o = case o of
  ROperand f -> do
    (s, fty) <- St.get
    let g = fmap h (f s fty)
        h (X.Operand fn) = Ly.TokOperand fn
    return (Just g)
  RMatcherSelect f -> do
    (c, _) <- St.get
    St.put (c, f)
    return Nothing
  RCaseSelect c -> do
    (_, f) <- St.get
    St.put (c, f)
    return Nothing
  ROperator t -> return . Just . return $ t
  _ -> return Nothing


makeTokens
  :: Defaults
  -> [OptResult]
  -> Ex.Exceptional String ( [Ly.Token (L.PostFam -> Bool)]
                           , (M.CaseSensitive, Factory) )
makeTokens df os =
  let initSt = (sensitive df, factory df)
      lsSt = mapM makeToken os
      (ls, st') = St.runState lsSt initSt
  in fmap (\xs -> (xs, st')) . sequence . catMaybes $ ls


allOpts :: L.DateTime -> [MA.OptSpec OptResult]
allOpts dt =
  map (fmap ROperand) (Ly.operandSpecs dt)
  ++ [fmap RPostFilter . fst $ Ly.postFilterSpecs]
  ++ [fmap RPostFilter . snd $ Ly.postFilterSpecs]
  ++ map (fmap RMatcherSelect) Ly.matcherSelectSpecs
  ++ map (fmap RCaseSelect) Ly.caseSelectSpecs
  ++ map (fmap ROperator) Ly.operatorSpecs
  ++ [(fmap RSortSpec) Ly.sortSpecs]
  ++ [ MA.OptSpec ["help"] "h" (MA.NoArg RHelp)
     , optScheme
     , optColorToFile ]

optColorToFile :: MA.OptSpec OptResult
optColorToFile = MA.OptSpec ["color-to-file"] "" (MA.ChoiceArg ls)
  where
    ls = [ ("yes", RColorToFile $ ColorToFile True)
         , ("no", RColorToFile $ ColorToFile False) ]

getColorToFile :: Defaults -> [OptResult] -> ColorToFile
getColorToFile d ls =
  case mapMaybe getOpt ls of
    [] -> colorToFile d
    xs -> last xs
  where
    getOpt o = case o of
      RColorToFile c -> Just c
      _ -> Nothing

optScheme :: MA.OptSpec OptResult
optScheme = MA.OptSpec ["scheme"] "" (MA.ChoiceArg ls)
  where
    ls = [ ("dark", RScheme Dark.scheme)
         , ("light", RScheme Light.scheme) ]

getScheme :: Defaults -> [OptResult] -> E.Scheme
getScheme d ls =
  case mapMaybe getOpt ls of
    [] -> scheme d
    xs -> last xs
  where
    getOpt o = case o of
      RScheme s -> Just s
      _ -> Nothing

data GlobalResult
  = NeedsHelp
  | RunPenny FilterOpts

-- | Indicates the result of a successful parse of filtering options.
data FilterOpts = FilterOpts
  { _resultFactory :: M.CaseSensitive
                     -> Text -> Ex.Exceptional Text (Text -> Bool)
    -- ^ The factory indicated, so that it can be used in
    -- subsequent parses of the same command line.

  , _resultSensitive :: M.CaseSensitive
    -- ^ Indicated case sensitivity, so that it can be used in
    -- subsequent parses of the command line.

  , _sorterFilterer :: [L.Transaction] -> [L.Box Ly.LibertyMeta]
    -- ^ Applied to a list of Transaction, will sort and filter
    -- the transactions and assign them LibertyMeta.

  , foScheme :: E.Scheme

  , foColorToFile :: ColorToFile
  }

processGlobal
  :: Defaults
  -> [OptResult]
  -> Ex.Exceptional String GlobalResult
processGlobal d os =
  if any isHelp os
  then return NeedsHelp
  else do
    postFilts <- getPostFilters os
    sortSpec <- getSortSpec os
    (toks, (rs, rf)) <- makeTokens d os
    let ctf = getColorToFile d os
        sch = getScheme d os
        err = "could not parse filter expression."
    pdct <- Ex.fromMaybe err $ Ly.parsePredicate toks
    let sf = Ly.xactionsToFiltered pdct postFilts sortSpec
        fo = FilterOpts rf rs sf sch ctf
    return $ RunPenny fo

--
-- Ledger parsing
--
warnTerminal :: IO ()
warnTerminal =
  hPutStrLn stderr $ "zinc: warning: reading from standard input, "
  ++ "which is a terminal"

data Filename =
  Filename Text
  | Stdin

-- | Converts a Ledgers filename to a Lincoln filename.
convertFilename :: Filename -> L.Filename
convertFilename (Filename x) = L.Filename x
convertFilename Stdin = L.Filename . pack $ "<stdin>"

-- | Actually reads the file off disk. For now just let this crash if
-- any of the IO errors occur.
ledgerText :: Filename -> IO Text
ledgerText f = case f of
  Stdin -> do
    isTerm <- hIsTerminalDevice stdin
    when isTerm warnTerminal
    TIO.hGetContents stdin
  Filename fn -> TIO.readFile (unpack fn)

-- | Converts a string from the command line to a Filename.
toFilename :: String -> Filename
toFilename s =
  if s == "-"
  then Stdin
  else Filename . pack $ s

readLedgers :: [String] -> IO [(Filename, Text)]
readLedgers ss =
  let fns = if null ss then [Stdin] else map toFilename ss
      f fn = (\txt -> (fn, txt)) <$> ledgerText fn
  in mapM f fns


parseLedgers
  :: [(Filename, Text)]
  -> Ex.Exceptional String ([L.Transaction], [L.PricePoint])
parseLedgers ls =
  let toPair (f, t) = (convertFilename f, C.FileContents t)
      parsed = C.parse (map toPair ls)
      folder i (ts, ps) = case i of
        C.Transaction t -> (t:ts, ps)
        C.PricePoint p -> (ts, p:ps)
        _ -> (ts, ps)
      toResult (C.Ledger is) = foldr folder ([], []) is
      toErr x = "could not parse ledger: "
                ++ (unpack . C.unErrorMsg $ x)
  in Ex.mapExceptional toErr toResult parsed


data DisplayOpts = DisplayOpts ColorToFile E.Scheme

toDisplayOpts :: FilterOpts -> DisplayOpts
toDisplayOpts o = DisplayOpts (foColorToFile o) (foScheme o)

parseCommandLine
  :: Defaults
  -> [I.Report]
  -> S.Runtime
  -> [String]
  -> Ex.Exceptional MA.Error
     (GlobalResult, Either [()] (DisplayOpts, I.ParseResult))
parseCommandLine df rs rt ss =
  MA.modes (allOpts (S.currentTime rt)) (processGlobal df)
           (whatMode rt rs) ss

whatMode
  :: S.Runtime
  -> [I.Report]
  -> GlobalResult
  -> Either (a -> ()) [MA.Mode (DisplayOpts, I.ParseResult)]
whatMode rt rs gr =
  case gr of
    NeedsHelp -> Left $ const ()
    RunPenny fo@(FilterOpts fty cs sf _ _) ->
      let prs = map snd rs
                <*> pure rt
                <*> pure cs
                <*> pure fty
                <*> pure sf
      in Right $ map (fmap (\r -> ((toDisplayOpts fo), r))) prs

handleParseResult
  :: S.Runtime
  -> [I.Report]
  -> Ex.Exceptional MA.Error
     (a, Either b (DisplayOpts, I.ParseResult))
  -> IO ()
handleParseResult rt rs r =
  let showErr e = do
        IO.hPutStr IO.stderr $ "penny: error: " ++ e
        exitFailure
  in case r of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr $ MA.formatError "penny" e
      exitFailure
    Ex.Success (_, ei) ->
      case ei of
        Left _ ->  putStr (helpText rs) >> exitSuccess
        Right ((DisplayOpts ctf sch), ex) -> case ex of
          Ex.Exception s -> showErr s
          Ex.Success good -> either showHelp runCmd good
            where
              showHelp h = putStr h >> exitSuccess
              runCmd (fns, pr) = do
                ledgers <- readLedgers fns
                (txns, pps) <- Ex.switch showErr return
                               $ parseLedgers ledgers
                let term = if unColorToFile ctf
                           then Chk.termFromEnv rt
                           else Chk.autoTerm rt
                Ex.switch (showErr . unpack)
                  (printChunks term sch) $ pr txns pps

printChunks
  :: Chk.Term
  -> E.Scheme
  -> [E.PreChunk]
  -> IO ()
printChunks t s =
  Chk.printChunks t
  . map (E.makeChunk s)

helpText ::
  [I.Report]
  -> String
helpText = mappend help . mconcat . fmap fst


parseAndPrint
  :: Defaults
  -> [I.Report]
  -> S.Runtime
  -> [String]
  -> IO ()
parseAndPrint df rs rt ss =
  handleParseResult rt rs
  $ parseCommandLine df rs rt ss

help :: String
help = unlines [
  "usage: penny [posting filters] report [report options] file . . .",
  "",
  "Posting filters",
  "------------------------------------------",
  "",
  "Dates",
  "-----",
  "",
  "--date cmp timespec, -d cmp timespec",
  "  Date must be within the time frame given. timespec",
  "  is a day or a day and a time. Valid values for cmp:",
  "     <, >, <=, >=, ==, /=, !=",
  "--current",
  "  Same as \"--date <= (right now) \"",
  "",
  "Serials",
  "----------------",
  "These options take the form --option cmp num; the given",
  "sequence number must fall within the given range. \"rev\"",
  "in the option name indicates numbering is from end to beginning.",
  "",
  "--globalTransaction, --revGlobalTransaction",
  "  All transactions, after reading the ledger files",
  "--globalPosting, --revGlobalPosting",
  "  All postings, after reading the leder files",
  "--fileTransaction, --revFileTransaction",
  "  Transactions in each ledger file, after reading the files",
  "  (numbering restarts with each file)",
  "--filePosting, --revFilePosting",
  "  Postings in each ledger file, after reading the files",
  "  (numbering restarts with each file)",
  "",
  "Pattern matching",
  "----------------",
  "",
  "-a pattern, --account pattern",
  "  Pattern must match colon-separated account name",
  "--account-level num pat",
  "  Pattern must match sub account at given level",
  "--account-any pat",
  "  Pattern must match sub account at any level",
  "-p pattern, --payee pattern",
  "  Payee must match pattern",
  "-t pattern, --tag pattern",
  "  Tag must match pattern",
  "--number pattern",
  "  Number must match pattern",
  "--flag pattern",
  "  Flag must match pattern",
  "--commodity pattern",
  "  Pattern must match colon-separated commodity name",
  "--posting-memo pattern",
  "  Posting memo must match pattern",
  "--transaction-memo pattern",
  "  Transaction memo must match pattern",
  "",
  "Other posting characteristics",
  "-----------------------------",
  "--debit",
  "  Entry must be a debit",
  "--credit",
  "  Entry must be a credit",
  "--qty cmp number",
  "  Entry quantity must fall within given range",
  "",
  "Operators - from highest to lowest precedence",
  "(all are left associative)",
  "--------------------------",
  "--open expr --close",
  "  Force precedence (as in \"open\" and \"close\" parentheses)",
  "--not expr",
  "  True if expr is false",
  "expr1 --and expr2 ",
  "  True if expr and expr2 are both true",
  "expr1 --or expr2",
  "  True if either expr1 or expr2 is true",
  "",
  "Options affecting patterns",
  "--------------------------",
  "",
  "-i, --case-insensitive",
  "  Be case insensitive (default)",
  "-I, --case-sensitive",
  "  Be case sensitive",
  "",
  "--within",
  "  Use \"within\" matcher (default)",
  "--pcre",
  "  Use \"pcre\" matcher",
  "--posix",
  "  Use \"posix\" matcher",
  "--exact",
  "  Use \"exact\" matcher",
  "",
  "Removing postings after sorting and filtering",
  "---------------------------------------------",
  "--head n",
  "  Keep only the first n postings",
  "--tail n",
  "  Keep only the last n postings",
  "",
  "Sorting",
  "-------",
  "",
  "-s key, --sort key",
  "  Sort postings according to key",
  "",
  "Keys:",
  "  payee, date, flag, number, account, drCr,",
  "  qty, commodity, postingMemo, transactionMemo",
  "",
  "  Ascending order by default; for descending order,",
  "  capitalize the name of the key.",
  "",
  "Colors",
  "------",
  "--color-to-file no|yes",
  "  Whether to use color when standard output is not a",
  "  terminal (default: no)",
  "--scheme SCHEME_NAME",
  "  use color scheme for report. Default available schemes:",
  "    dark - for dark terminal background (default)",
  "    light - for light terminal background"
  ]
