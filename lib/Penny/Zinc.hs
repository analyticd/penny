{-# LANGUAGE OverloadedStrings #-}

-- | Zinc - the Penny command-line interface
module Penny.Zinc
  ( Defaults(..)
  , ColorToFile(..)
  , Matcher(..)
  , SortField(..)
  , runZinc
  ) where

import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Scheme.Schemes as Schemes
import qualified Penny.Copper as C
import qualified Penny.Liberty as Ly
import qualified Data.Prednote.Expressions as X
import qualified Data.Prednote.Pdct as Pe
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Shield as S
import qualified Penny.Steel.Sums as Su

import Control.Applicative ((<*>), pure, (<$))
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toUpper, toLower)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Monoid (mappend, mconcat, (<>))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import Data.Version (Version)
import qualified Data.Text.IO as TIO
import qualified System.Console.MultiArg as MA
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Matchers as M
import qualified System.Console.Rainbow as R

runZinc
  :: Version
  -- ^ Version of the executable
  -> Defaults
  -> S.Runtime
  -> [I.Report]
  -> IO ()
runZinc ver df rt rs = do
  let ord = sortPairsToFn . sorter $ df
      hlp = helpText df rt rs
      globals = MA.optsHelpVersion hlp (Ly.version ver)
                (allOpts (S.currentTime rt) df)
  ei <- MA.modesIO globals (processGlobal rt ord df rs)
  case ei of
    Left h -> do
      pn <- MA.getProgName
      putStr (h pn)
      Exit.exitSuccess
    Right act -> act


-- | Whether to use color when standard output is not a terminal.
newtype ColorToFile = ColorToFile { unColorToFile :: Bool }
  deriving (Eq, Show)

data Matcher
  = Within
  | Exact
  | PCRE
  deriving (Eq, Show)

data SortField
  = Payee
  | Date
  | Flag
  | Number
  | Account
  | DrCr
  | Qty
  | Commodity
  | PostingMemo
  | TransactionMemo
  deriving (Eq, Show, Ord)

data Defaults = Defaults
  { sensitive :: M.CaseSensitive
  , matcher :: Matcher
  , colorToFile :: ColorToFile
  , defaultScheme :: Maybe E.Scheme
    -- ^ If Nothing, no default scheme. If the user does not pick a
    -- scheme, no colors are used.
  , moreSchemes :: [E.Scheme]
  , sorter :: [(SortField, P.SortOrder)]
    -- ^ For example, to sort by date and then by payee if the dates
    -- are equal, use
    --
    -- > [(Date, Ascending), (Payee, Ascending)]

  , exprDesc :: X.ExprDesc
  , formatQty :: [C.LedgerItem] -> L.Amount L.Qty -> Text
  }

sortPairToFn :: (SortField, P.SortOrder) -> Orderer
sortPairToFn (s, d) = if d == P.Descending then flipOrder r else r
  where
    r = case s of
      Payee -> comparing Q.payee
      Date -> comparing Q.dateTime
      Flag -> comparing Q.flag
      Number -> comparing Q.number
      Account -> comparing Q.account
      DrCr -> comparing Q.drCr
      Qty -> comparing Q.qty
      Commodity -> comparing Q.commodity
      PostingMemo -> comparing Q.postingMemo
      TransactionMemo -> comparing Q.transactionMemo

descPair :: (SortField, P.SortOrder) -> String
descPair (i, d) = desc ++ ", " ++ dir
  where
    dir = case d of
      P.Ascending -> "ascending"
      P.Descending -> "descending"
    desc = case show i of
      [] -> []
      x:xs -> toLower x : xs

descSortList :: [(SortField, P.SortOrder)] -> [String]
descSortList ls = case ls of
  [] -> ["    No sorting performed by default"]
  x:xs -> descFirst x : map descRest xs

descFirst :: (SortField, P.SortOrder) -> String
descFirst p = "  Default sort order: " ++ descPair p

descRest :: (SortField, P.SortOrder) -> String
descRest p = "    then: " ++ descPair p

sortPairsToFn :: [(SortField, P.SortOrder)] -> Orderer
sortPairsToFn = mconcat . map sortPairToFn

--
-- ## Option parsing
--

--
-- ## OptResult, and functions dealing with it
--
newtype ShowExpression = ShowExpression Bool
  deriving (Show, Eq)

newtype VerboseFilter = VerboseFilter Bool
  deriving (Show, Eq)

type Error = Text

data OptResult
  = ROperand (M.CaseSensitive
             -> Ly.MatcherFactory
             -> Ex.Exceptional Ly.Error Ly.Operand)
  | RPostFilter (Ex.Exceptional Ly.Error Ly.PostFilterFn)
  | RMatcherSelect Ly.MatcherFactory
  | RCaseSelect M.CaseSensitive
  | ROperator (X.Token L.Posting)
  | RSortSpec (Ex.Exceptional Error Orderer)
  | RColorToFile ColorToFile
  | RScheme E.Changers
  | RExprDesc X.ExprDesc
  | RShowExpression
  | RVerboseFilter

getPostFilters
  :: [OptResult]
  -> Ex.Exceptional Ly.Error [Ly.PostFilterFn]
getPostFilters =
  sequence
  . mapMaybe f
  where
    f o = case o of
      RPostFilter pf -> Just pf
      _ -> Nothing

getExprDesc
  :: Defaults
  -> [OptResult]
  -> X.ExprDesc
getExprDesc df os = case mapMaybe f os of
  [] -> exprDesc df
  xs -> last xs
  where
    f (RExprDesc d) = Just d
    f _ = Nothing

getSortSpec
  :: Orderer
  -> [OptResult]
  -> Ex.Exceptional Error Orderer
getSortSpec i ls =
  let getSpec o = case o of
        RSortSpec x -> Just x
        _ -> Nothing
      exSpecs = mapMaybe getSpec ls
  in if null exSpecs
     then return i
     else fmap mconcat . sequence $ exSpecs

type Factory = M.CaseSensitive
             -> Text -> Ex.Exceptional Text M.Matcher

makeToken
  :: OptResult
  -> St.State (M.CaseSensitive, Factory)
              (Maybe (Ex.Exceptional Ly.Error (X.Token L.Posting)))
makeToken o = case o of
  ROperand f -> do
    (s, fty) <- St.get
    let g = fmap X.operand (f s fty)
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
  -> Ex.Exceptional Ly.Error ( [X.Token L.Posting]
                             , (M.CaseSensitive, Factory) )
makeTokens df os =
  let initSt = (sensitive df, fty)
      fty = case matcher df of
        Within -> \c t -> return (M.within c t)
        Exact -> \c t -> return (M.exact c t)
        PCRE -> \c t -> Ex.fromEither $ M.pcre c t
      lsSt = mapM makeToken os
      (ls, st') = St.runState lsSt initSt
  in fmap (\xs -> (xs, st')) . sequence . catMaybes $ ls


allOpts :: L.DateTime -> Defaults -> [MA.OptSpec OptResult]
allOpts dt df =
  map (fmap ROperand) (Ly.operandSpecs dt)
  ++ [fmap RPostFilter . fst $ Ly.postFilterSpecs]
  ++ [fmap RPostFilter . snd $ Ly.postFilterSpecs]
  ++ map (fmap RMatcherSelect) Ly.matcherSelectSpecs
  ++ map (fmap RCaseSelect) Ly.caseSelectSpecs
  ++ map (fmap ROperator) Ly.operatorSpecs
  ++ [fmap RSortSpec sortSpecs]
  ++ [ optColorToFile ]
  ++ let ss = moreSchemes df
     in (if not . null $ ss then [optScheme ss] else [])
  ++ map (fmap RExprDesc) Ly.exprDesc
  ++ [ RShowExpression <$ Ly.showExpression
     , RVerboseFilter <$ Ly.verboseFilter
     ]

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

optScheme :: [E.Scheme] -> MA.OptSpec OptResult
optScheme ss = MA.OptSpec ["scheme"] "" (MA.ChoiceArg ls)
  where
    ls = map f ss
    f (E.Scheme n _ s) = (n, RScheme s)

getScheme :: Defaults -> [OptResult] -> Maybe E.Changers
getScheme d ls =
  case mapMaybe getOpt ls of
    [] -> fmap E.changers $ defaultScheme d
    xs -> Just $ last xs
  where
    getOpt o = case o of
      RScheme s -> Just s
      _ -> Nothing

getShowExpression :: [OptResult] -> ShowExpression
getShowExpression ls = case mapMaybe f ls of
  [] -> ShowExpression False
  _ -> ShowExpression True
  where
    f o = case o of { RShowExpression -> Just (); _ -> Nothing }

getVerboseFilter :: [OptResult] -> VerboseFilter
getVerboseFilter ls = case mapMaybe f ls of
  [] -> VerboseFilter False
  _ -> VerboseFilter True
  where
    f o = case o of { RVerboseFilter -> Just (); _ -> Nothing }

-- | Indicates the result of a successful parse of filtering options.
data FilterOpts = FilterOpts
  { foResultFactory :: Factory
    -- ^ The factory indicated, so that it can be used in
    -- subsequent parses of the same command line.

  , foResultSensitive :: M.CaseSensitive
    -- ^ Indicated case sensitivity, so that it can be used in
    -- subsequent parses of the command line.

  , foSorterFilterer :: [L.Transaction]
                     -> ( (L.Amount L.Qty -> Text) -> [R.Chunk]
                          , [(Ly.LibertyMeta, L.Posting)])
    -- ^ Applied to a list of Transaction, will sort and filter the
    -- transactions and assign them LibertyMeta. Also, returns a
    -- function that, when applied to a function that will format
    -- quantities, returns a verbose description of what was filtered.

  , foTextSpecs :: Maybe E.Changers

  , foColorToFile :: ColorToFile
  , foExprDesc :: X.ExprDesc
  , foPredicate :: Pe.Pdct L.Posting
  , foShowExpression :: ShowExpression
  , foVerboseFilter :: VerboseFilter
  }

processGlobal
  :: S.Runtime
  -> Orderer
  -> Defaults
  -> [I.Report]
  -> [OptResult]
  -> Ex.Exceptional String
      (Either a [MA.Mode (MA.ProgName -> String) (IO ())])
processGlobal rt srt df rpts os
  = case processFiltOpts srt df os of
      Ex.Exception s -> Ex.throw s
      Ex.Success fo ->
        return . Right $ map (makeMode (formatQty df) rt fo) rpts

processFiltOpts
  :: Orderer
  -> Defaults
  -> [OptResult]
  -> Ex.Exceptional String FilterOpts
processFiltOpts ord df os = Ex.mapException unpack $ do
  postFilts <- getPostFilters os
  sortSpec <- getSortSpec ord os
  (toks, (rs, rf)) <- makeTokens df os
  let ctf = getColorToFile df os
      sch = getScheme df os
      expDsc = getExprDesc df os
      showExpr = getShowExpression os
      verbFilt = getVerboseFilter os
  pdct <- Ly.parsePredicate expDsc toks
  let sf ls = Ly.xactionsToFiltered pdct postFilts sortSpec ls
  return $ FilterOpts rf rs sf sch
                      ctf expDsc pdct showExpr verbFilt

makeMode
  :: ([C.LedgerItem] -> L.Amount L.Qty -> Text)
  -> S.Runtime
  -> FilterOpts
  -> I.Report
  -> MA.Mode (MA.ProgName -> String) (IO ())
makeMode mkFmt rt fo r = fmap makeIO mode
  where
    mode = snd (r rt) (foResultSensitive fo) (foResultFactory fo)
           (fromMaybe Schemes.plainLabels . foTextSpecs $ fo)
           (foExprDesc fo) (fmap snd (foSorterFilterer fo))
    makeIO parseResult = do
      (posArgs, printRpt) <-
        Ex.switch handleTextError return parseResult
      items <- C.open posArgs
      let fmt = mkFmt items
          (txns, pps) = splitLedger items
          term = if unColorToFile (foColorToFile fo)
                 then S.termFromEnv rt
                 else S.autoTerm rt
          printer = R.putChunks term
          verbFiltChunks = (fst . foSorterFilterer fo $ txns) fmt
      showFilterExpression printer (foShowExpression fo) (foPredicate fo)
      showVerboseFilter printer (foVerboseFilter fo) verbFiltChunks
      Ex.switch handleTextError (R.putChunks term)
        $ printRpt fmt txns pps


handleTextError :: Text -> IO a
handleTextError x = do
  pn <- MA.getProgName
  TIO.hPutStr IO.stderr $ (pack pn) <> ": error: " <> x
  Exit.exitFailure

indentAmt :: Pe.IndentAmt
indentAmt = 4

blankLine :: R.Chunk
blankLine = "\n"

showFilterExpression
  :: ([R.Chunk] -> IO ())
  -> ShowExpression
  -> Pe.Pdct L.Posting
  -> IO ()
showFilterExpression ptr (ShowExpression se) pdct =
  if not se
  then return ()
  else ptr $ info : blankLine :
             (Pe.showPdct indentAmt 0 pdct ++ [blankLine])
  where
    info = "Posting filter expression:\n"

showVerboseFilter
  :: ([R.Chunk] -> IO ())
  -> VerboseFilter
  -> [R.Chunk]
  -> IO ()
showVerboseFilter ptr (VerboseFilter vb) cks =
  if not vb
  then return ()
  else ptr $ info : blankLine : (cks ++ [blankLine])
  where
    info = "Filtering information:\n"

-- | Splits a Ledger into its Transactions and PricePoints.
splitLedger :: [C.LedgerItem] -> ([L.Transaction], [L.PricePoint])
splitLedger = partitionEithers . mapMaybe toEither
  where
    toEither = Su.caseS4 (Just  . Left) (Just . Right)
                         (const Nothing) (const Nothing)

helpText
  :: Defaults
  -> S.Runtime
  -> [I.Report]
  -> String
  -> String
helpText df rt pairMakers pn =
  mappend (help df pn) . mconcat . map addHdr . fmap fst $ pairs
  where
    pairs = pairMakers <*> pure rt
    addHdr s = hdr ++ s
    hdr = unlines [ "", replicate 50 '=' ]


------------------------------------------------------------
-- ## Sorting
------------------------------------------------------------

-- The monoid instance of Ordering takes the first non-EQ item. For
-- example:
--
-- mconcat [EQ, LT, GT] == LT.
--
-- If b is a monoid, then (a -> b) is also a monoid. Therefore (a -> a
-- -> Ordering) is also a monoid. So for example to compare the first
-- element of a pair and then by the second element only if the first
-- element is equal:
--
-- mconcat [comparing fst, comparing snd]

type Orderer = L.Posting -> L.Posting -> Ordering

flipOrder :: (a -> a -> Ordering) -> (a -> a -> Ordering)
flipOrder f = f' where
  f' p1 p2 = case f p1 p2 of
    LT -> GT
    GT -> LT
    EQ -> EQ

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter s = case s of
  [] -> []
  (x:xs) -> toUpper x : xs

ordPairs :: [(String, Orderer)]
ordPairs =
  [ ("payee", comparing Q.payee)
  , ("date", comparing Q.dateTime)
  , ("flag", comparing Q.flag)
  , ("number", comparing Q.number)
  , ("account", comparing Q.account)
  , ("drCr", comparing Q.drCr)
  , ("qty", comparing Q.qty)
  , ("commodity", comparing Q.commodity)
  , ("postingMemo", comparing Q.postingMemo)
  , ("transactionMemo", comparing Q.transactionMemo) ]

ords :: [(String, Orderer)]
ords = ordPairs ++ uppers ++ [none] where
  uppers = map toReversed ordPairs
  toReversed (s, f) =
    (capitalizeFirstLetter s, flipOrder f)
  none = ("none", const . const $ EQ)


-- | True if the first argument matches the second argument. The match
-- on the first letter is case sensitive; the match on the other
-- letters is not case sensitive. True if both strings are empty.
argMatch :: String -> String -> Bool
argMatch s1 s2 = case (s1, s2) of
  (x:xs, y:ys) ->
    (x == y) && ((map toUpper xs) `isPrefixOf` (map toUpper ys))
  _ -> True

sortSpecs :: MA.OptSpec (Ex.Exceptional Error Orderer)
sortSpecs = MA.OptSpec ["sort"] ['s'] (MA.OneArg f)
  where
    f a =
      let matches = filter (\p -> a `argMatch` (fst p)) ords
      in case matches of
        x:[] -> return $ snd x
        _ -> Ex.throw $ "bad sort specification: " <> pack a <> "\n"



------------------------------------------------------------
-- ## Help
------------------------------------------------------------

help :: Defaults -> String -> String
help d pn = unlines $
  [ "usage: " ++ pn ++ " [posting filters] report [report options] file . . ."
  , ""
  , "Posting filters"
  , "------------------------------------------"
  , ""
  , "Dates"
  , "-----"
  , ""
  , "-d, --date cmp timespec"
  , "  Date must be within the time frame given. timespec"
  , "  is a day or a day and a time. Valid values for cmp:"
  , "     <, >, <=, >=, ==, /=, !="
  , "--current"
  , "  Same as \"--date <= (right now) \""
  , ""
  , "Serials"
  , "----------------"
  , "These options take the form --option cmp num; the given"
  , "sequence number must fall within the given range. \"rev\""
  , "in the option name indicates numbering is from end to beginning."
  , ""
  , "--globalTransaction, --revGlobalTransaction"
  , "  All transactions, after reading the ledger files"
  , "--globalPosting, --revGlobalPosting"
  , "  All postings, after reading the leder files"
  , "--fileTransaction, --revFileTransaction"
  , "  Transactions in each ledger file, after reading the files"
  , "  (numbering restarts with each file)"
  , "--filePosting, --revFilePosting"
  , "  Postings in each ledger file, after reading the files"
  , "  (numbering restarts with each file)"
  , ""
  , "Pattern matching"
  , "----------------"
  , ""
  , "-a pattern, --account pattern"
  , "  Pattern must match colon-separated account name"
  , "--account-level num pat"
  , "  Pattern must match sub account at given level"
  , "--account-any pat"
  , "  Pattern must match sub account at any level"
  , "-p pattern, --payee pattern"
  , "  Payee must match pattern"
  , "-t pattern, --tag pattern"
  , "  Tag must match pattern"
  , "-n, --number pattern"
  , "  Number must match pattern"
  , "-f, --flag pattern"
  , "  Flag must match pattern"
  , "-y, --commodity pattern"
  , "  Pattern must match commodity name"
  , "--posting-memo pattern"
  , "  Posting memo must match pattern"
  , "--transaction-memo pattern"
  , "  Transaction memo must match pattern"
  , ""
  , "Other posting characteristics"
  , "-----------------------------"
  , "--debit"
  , "  Entry must be a debit"
  , "--credit"
  , "  Entry must be a credit"
  , "-q, --qty cmp number"
  , "  Entry quantity must fall within given range"
  , "--filename pattern"
  , "  Filename of posting must match pattern"
  , ""
  , "Filtering based upon sibling postings"
  , "-------------------------------------"
  , "--s-globalPosting"
  , "--s-revGlobalPosting"
  , "--s-filePosting"
  , "--s-revFilePosting"
  , "--s-account"
  , "--s-account-level"
  , "--s-account-any"
  , "--s-payee"
  , "--s-tag"
  , "--s-number"
  , "--s-flag"
  , "--s-commodity"
  , "--s-posting-memo"
  , "--s-debit"
  , "--s-credit"
  , "--s-qty"
  , ""
  , "Options affecting patterns"
  , "--------------------------"
  , ""

  , "-i, --case-insensitive"
  , "  Be case insensitive"
    ++ ifDefault (sensitive d == M.Insensitive)

  , "-I, --case-sensitive"
  , "  Be case sensitive"
    ++ ifDefault (sensitive d == M.Sensitive)

  , ""

  , "-w, --within"
  , "  Use \"within\" matcher"
    ++ ifDefault (matcher d == Within)

  , "-r, --pcre"
  , "  Use \"pcre\" matcher"
    ++ ifDefault (matcher d == PCRE)

  , "-x, --exact"
  , "  Use \"exact\" matcher"
    ++ ifDefault (matcher d == Exact)
  , ""
  , "Infix or RPN selection"
  , "----------------------"
  , "--infix - use infix notation"
    ++ ifDefault (exprDesc d == X.Infix)
  , "--rpn - use reverse polish notation"
    ++ ifDefault (exprDesc d == X.RPN)
  , ""
  , "Infix Operators - from highest to lowest precedence"
  , "(all are left associative)"
  , "--------------------------"
  , "--open expr --close"
  , "-( expr -)"
  , "  Force precedence (as in \"open\" and \"close\" parentheses)"
  , "--not, -N expr"
  , "  True if expr is false"
  , "expr1 --and expr2"
  , "expr -A expr2"
  , "  True if expr and expr2 are both true"
  , "expr1 --or expr2"
  , "expr1 -O expr2"
  , "  True if either expr1 or expr2 is true"
  , ""
  , "RPN Operators"
  , "-------------"
  , "--not, -N"
  , "--and, -A"
  , "--or, -O"
  , "  RPN counterparts to the infix operators"
  , "  are postfix and manipulate the RPN stack accordingly"
  , ""
  , "Showing expressions"
  , "-------------------"
  , "--show-expression"
  , "  Show the parsed filter expression"
  , "--verbose-filter"
  , "  Verbosely show filtering results"
  , ""
  , "Removing postings after sorting and filtering"
  , "---------------------------------------------"
  , "--head n"
  , "  Keep only the first n postings"
  , "--tail n"
  , "  Keep only the last n postings"
  , ""
  , "Sorting"
  , "-------"
  , ""
  , "-s key, --sort key"
  , "  Sort postings according to key"
  , ""
  , "Keys:"
  , "  payee, date, flag, number, account, drCr,"
  , "  qty, commodity, postingMemo, transactionMemo"
  , ""
  , "  Ascending order by default; for descending order,"
  , "  capitalize the name of the key."
  , "  (use \"none\" to leave postings in ledger file order)"
  , ""
  ] ++ descSortList (sorter d) ++
  [ ""
  , "Colors"
  , "------"
  , "default scheme:"
  ,  maybe "    (none)" descScheme (defaultScheme d)
  , ""
  ]
  ++ let schs = moreSchemes d
     in (if not . null $ schs
        then
          [ "--scheme SCHEME_NAME"
          , "  use color scheme for report. Available schemes:"
          ] ++ map descScheme schs
        else [])
  ++
  [ ""
  , "--color-to-file no|yes"
  , "  Whether to use color when standard output is not a"
  , "  terminal (default: " ++
    if unColorToFile . colorToFile $ d then "yes)" else "no)"
  , ""
  , "Meta"
  , "----"
  , "--help, -h - show this help and exit"
  , "--version - show version and exit"
  ]


descScheme :: E.Scheme -> String
descScheme (E.Scheme n d _) = "    " ++ n ++ " - " ++ d

-- | The string @ (default)@ if the condition is True; otherwise,
-- nothing.
ifDefault :: Bool -> String
ifDefault b = if b then " (default)" else ""
