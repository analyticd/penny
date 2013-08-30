{-# LANGUAGE OverloadedStrings, CPP #-}

-- | Liberty - Penny command line parsing utilities
--
-- Both Cabin and Zinc share various functions that aid in parsing
-- command lines. For instance both the Postings report and the Zinc
-- postings filter use common command-line options. However, Zinc
-- already depends on Cabin. To avoid a cyclic dependency whereby
-- Cabin would also depend on Zinc, functions formerly in Zinc that
-- Cabin will also find useful are relocated here, to Liberty.

module Penny.Liberty (
  MatcherFactory,
  FilteredNum(FilteredNum, unFilteredNum),
  SortedNum(SortedNum, unSortedNum),
  LibertyMeta(filteredNum, sortedNum),
  xactionsToFiltered,
  ListLength(ListLength, unListLength),
  ItemIndex(ItemIndex, unItemIndex),
  PostFilterFn,
  parseComparer,
  processPostFilters,
  parsePredicate,
  parseInt,
  parseInfix,
  parseRPN,
  exprDesc,
  showExpression,
  verboseFilter,

  -- * Parsers
  Operand,
  operandSpecs,
  postFilterSpecs,
  matcherSelectSpecs,
  caseSelectSpecs,
  operatorSpecs,

  -- * Version
  version,


  -- * Output
  output,
  processOutput,

  -- * Errors
  Error

  ) where

import Control.Arrow (first, second)
import Control.Applicative ((<*>), (<$>), pure, Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Text (Text, pack)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified System.Console.MultiArg as MA
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Combinator (OptSpec)
import Text.Parsec (parse)

import qualified Penny.Copper.Parsec as Pc

import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Predicates.Siblings as PS
import qualified Data.Prednote.Pdct as E
import qualified Penny.Lincoln as L
import qualified System.Console.Rainbow as C
import qualified Data.Prednote.Expressions as X

import Text.Matchers (
  CaseSensitive(Sensitive, Insensitive))
import qualified Text.Matchers as TM

#ifdef incabal
import qualified Paths_penny as PPL
#endif
import qualified Data.Version as V

-- | A multiline Text that holds an error message.
type Error = Text

-- | A serial indicating how a post relates to all other postings that
-- made it through the filtering phase.
newtype FilteredNum = FilteredNum { unFilteredNum :: L.Serial }
                      deriving Show

-- | A serial indicating how a posting relates to all other postings
-- that have been sorted.
newtype SortedNum = SortedNum { unSortedNum :: L.Serial }
                    deriving Show

-- | All metadata from Liberty.
data LibertyMeta =
  LibertyMeta { filteredNum :: FilteredNum
              , sortedNum :: SortedNum }
  deriving Show


-- | Parses a list of tokens to obtain a predicate. Deals with an
-- empty list of tokens by returning a predicate that is always
-- True. Fails if the list of tokens is not empty and the parse fails.
parsePredicate
  :: X.ExprDesc
  -> [X.Token a]
  -> Ex.Exceptional Error (E.Pdct a)
parsePredicate d ls = case ls of
  [] -> return E.always
  _ -> Ex.fromEither $ X.parseExpression d ls

-- | Takes a list of transactions, splits them into PostingChild
-- instances, filters them, post-filters them, sorts them, and places
-- them in Box instances with Filtered serials. Also returns Chunks
-- containing a description of the evalutation process.

xactionsToFiltered

  :: P.LPdct
  -- ^ The predicate to filter the transactions

  -> [PostFilterFn]
  -- ^ Post filter specs

  -> (L.Posting -> L.Posting -> Ordering)
  -- ^ The sorter

  -> [L.Transaction]
  -- ^ The transactions to work on (probably parsed in from Copper)

  -> ( (L.Amount L.Qty -> X.Text) -> [C.Chunk]
     , [(LibertyMeta, L.Posting)])
  -- ^ Sorted, filtered postings

xactionsToFiltered pdct postFilts srtr
  = second (processPostings srtr postFilts)
  . mainFilter pdct
  . concatMap L.transactionToPostings

processPostings
  :: (L.Posting -> L.Posting -> Ordering)
  -> [PostFilterFn]
  -> [L.Posting]
  -> [(LibertyMeta, L.Posting)]
processPostings srtr postFilters
  = (map . first . uncurry $ LibertyMeta)
  . addSortedNum
  . sortBy (\p1 p2 -> srtr (snd p1) (snd p2))
  . processPostFilters postFilters
  . addFilteredNum

mainFilter
  :: P.LPdct
  -> [L.Posting]
  -> ((L.Amount L.Qty -> X.Text) -> [C.Chunk], [L.Posting])
mainFilter pdct pstgs = (getChks, ps')
  where
    ps' = E.filter pdct pstgs
    getChks fmt = fst $ E.verboseFilter (L.display fmt) indentAmt
                        False pdct pstgs


addFilteredNum :: [a] -> [(FilteredNum, a)]
addFilteredNum = L.serialItems (\s p -> (FilteredNum s, p))

addSortedNum :: [(a, b)] -> [((a, SortedNum), b)]
addSortedNum = L.serialItems (\s (a, b) -> ((a, SortedNum s), b))

indentAmt :: E.IndentAmt
indentAmt = 4

type MatcherFactory
  = CaseSensitive
  -> Text
  -> Ex.Exceptional Text TM.Matcher

newtype ListLength = ListLength { unListLength :: Int }
                     deriving (Eq, Ord, Show)
newtype ItemIndex = ItemIndex { unItemIndex :: Int }
                    deriving (Eq, Ord, Show)

-- | Specifies options for the post-filter stage.
type PostFilterFn = ListLength -> ItemIndex -> Bool


processPostFilters :: [PostFilterFn] -> [a] -> [a]
processPostFilters pfs ls = foldl processPostFilter ls pfs


processPostFilter :: [a] -> PostFilterFn -> [a]
processPostFilter as fn = map fst . filter fn' $ zipped where
  len = ListLength $ length as
  fn' (_, idx) = fn len (ItemIndex idx)
  zipped = zip as [0..]


------------------------------------------------------------
-- Operands
------------------------------------------------------------

-- | Given a String from the command line which represents a pattern,
-- the current case sensitivity, and a MatcherFactory, return a
-- Matcher. Fails if the pattern is bad (e.g. it is not a valid
-- regular expression).
getMatcher
  :: String
  -> CaseSensitive
  -> MatcherFactory
  -> Ex.Exceptional Error TM.Matcher

getMatcher s cs f
  = Ex.mapException mkError
  $ f cs (pack s)
  where
    mkError eMsg = "bad pattern: \"" <> pack s <> " - " <> eMsg
      <> "\n"


-- | Parses comparers given on command line to a function. Fails if
-- the string given is invalid.
parseComparer
  :: String
  -> (Ordering -> E.Pdct a)
  -> Ex.Exceptional Error (E.Pdct a)
parseComparer s f = Ex.fromMaybe ("bad comparer: " <> pack s <> "\n")
                  $ E.parseComparer (pack s) f

-- | Parses a date from the command line. On failure, throws back the
-- error message from the failed parse.
parseDate :: String -> Ex.Exceptional Error Time.UTCTime
parseDate arg =
  Ex.mapExceptional err L.toUTC
  . Ex.fromEither
  . parse Pc.dateTime ""
  . pack
  $ arg
  where
    err msg = "bad date: \"" <> pack arg <> "\" - " <> (pack . show $ msg)

type Operand = E.Pdct L.Posting

-- | OptSpec for a date.
date :: OptSpec (Ex.Exceptional Error Operand)
date = C.OptSpec ["date"] ['d'] (C.TwoArg f)
  where
    f a1 a2 = do
      utct <- parseDate a2
      parseComparer a1 (flip P.date utct)


current :: L.DateTime -> OptSpec Operand
current dt = C.OptSpec ["current"] [] (C.NoArg f)
  where
    f = E.or [P.date LT (L.toUTC dt), P.date EQ (L.toUTC dt)]

-- | Parses exactly one integer; fails if it cannot read exactly one.
parseInt :: String -> Ex.Exceptional Error Int
parseInt t =
  case reads t of
    ((i, ""):[]) -> return i
    _ -> Ex.throw $ "could not parse integer: \"" <> pack t <> "\"\n"


-- | Creates options that add an operand that matches the posting if a
-- particluar field matches the pattern given.
patternOption ::
  String
  -- ^ Long option

  -> Maybe Char
  -- ^ Short option, if included

  -> (TM.Matcher -> P.LPdct)
  -- ^ When applied to a Matcher, this function returns a predicate.

  -> OptSpec ( CaseSensitive
               -> MatcherFactory
               -> Ex.Exceptional Error Operand )
patternOption str mc f = C.OptSpec [str] so (C.OneArg g)
  where
    so = maybe [] (:[]) mc
    g a1 cs fty = f <$> getMatcher a1 cs fty


-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: OptSpec ( CaseSensitive
                   -> MatcherFactory
                   -> Ex.Exceptional Error Operand )
account = C.OptSpec ["account"] "a" (C.OneArg f)
  where
    f a1 cs fty
      = fmap P.account
      $ getMatcher a1 cs fty


-- | The account-level option; matches if the account at the given
-- level matches.
accountLevel :: OptSpec ( CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional Error Operand)
accountLevel = C.OptSpec ["account-level"] "" (C.TwoArg f)
  where
    f a1 a2 cs fty
      = P.accountLevel <$> parseInt a1 <*> getMatcher a2 cs fty


-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: OptSpec ( CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional Error Operand )
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional Error Operand )
payee = patternOption "payee" (Just 'p') P.payee

tag :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional Error Operand)
tag = patternOption "tag" (Just 't') P.tag

number :: OptSpec ( CaseSensitive
                    -> MatcherFactory
                    -> Ex.Exceptional Error Operand )
number = patternOption "number" (Just 'n') P.number

flag :: OptSpec ( CaseSensitive
                  -> MatcherFactory
                  -> Ex.Exceptional Error Operand)
flag = patternOption "flag" (Just 'f') P.flag

commodity :: OptSpec ( CaseSensitive
                       -> MatcherFactory
                       -> Ex.Exceptional Error Operand)
commodity = patternOption "commodity" (Just 'y') P.commodity

filename :: OptSpec ( CaseSensitive
                      -> MatcherFactory
                      -> Ex.Exceptional Error Operand )
filename = patternOption "filename" Nothing P.filename

postingMemo :: OptSpec ( CaseSensitive
                         -> MatcherFactory
                         -> Ex.Exceptional Error Operand)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: OptSpec ( CaseSensitive
                             -> MatcherFactory
                             -> Ex.Exceptional Error Operand)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

debit :: OptSpec Operand
debit = C.OptSpec ["debit"] [] (C.NoArg P.debit)

credit :: OptSpec Operand
credit = C.OptSpec ["credit"] [] (C.NoArg P.credit)

qtyOption :: OptSpec (Ex.Exceptional Error Operand)
qtyOption = C.OptSpec ["qty"] "q" (C.TwoArg f)
  where
    f a1 a2 = do
      qt <- parseQty a2
      parseComparer a1 (flip P.qty qt)
    parseQty a = case parse Pc.unquotedQtyRepWithSpaces "" (pack a) of
      Left _ -> Ex.throw $ "failed to parse quantity"
      Right g -> pure . L.toQty $ g


-- | Creates two options suitable for comparison of serial numbers,
-- one for ascending, one for descending.
serialOption ::

  (L.Posting -> Maybe L.Serial)
  -- ^ Function that, when applied to a Posting, returns the serial
  -- you are interested in.

  -> String
  -- ^ Name of the command line option, such as @global-transaction@

  -> ( OptSpec (Ex.Exceptional Error Operand)
     , OptSpec (Ex.Exceptional Error Operand) )
  -- ^ Parses both descending and ascending serial options.

serialOption getSerial n = (osA, osD)
  where
    osA = C.OptSpec [n] []
          (C.TwoArg (f n L.forward))
    osD = let name = addPrefix "rev" n
          in C.OptSpec [name] []
             (C.TwoArg (f name L.backward))
    f name getInt a1 a2 = do
      num <- parseInt a2
      let getPdct = E.compareByMaybe (pack . show $ num) (pack name) cmp
          cmp l = case getSerial l of
            Nothing -> Nothing
            Just ser -> Just $ compare (getInt ser) num
      parseComparer a1 getPdct


-- | Creates two options suitable for comparison of sibling serial
-- numbers. Similar to serialOption.
siblingSerialOption
  :: String
  -- ^ Name of the command line option, such as @global-posting@

  -> (Int -> Ordering -> E.Pdct L.Posting)
  -- ^ Function that returns a Pdct for forward serial

  -> (Int -> Ordering -> E.Pdct L.Posting)
  -- ^ Function that returns a Pdct for reverse serial

  -> ( OptSpec (Ex.Exceptional Error Operand)
     , OptSpec (Ex.Exceptional Error Operand) )
  -- ^ Parses both descending and ascending serial options.

siblingSerialOption n fFwd fBak = (osA, osD)
  where
    osA = C.OptSpec ["s-" ++ n] [] (C.TwoArg (f fFwd))
    osD = let name = addPrefix "rev" n
          in C.OptSpec ["s-" ++ name] [] (C.TwoArg (f fBak))
    f getPdct a1 a2 = do
      num <- parseInt a2
      parseComparer a1 (getPdct num)


-- | Takes a string, adds a prefix and capitalizes the first letter of
-- the old string. e.g. applied to "rev" and "globalTransaction",
-- returns "revGlobalTransaction".
addPrefix :: String -> String -> String
addPrefix pre suf = pre ++ suf' where
  suf' = case suf of
    "" -> ""
    x:xs -> toUpper x : xs

globalTransaction :: ( OptSpec (Ex.Exceptional Error Operand)
                     , OptSpec (Ex.Exceptional Error Operand) )
globalTransaction =
  let f = fmap L.unGlobalTransaction . Q.globalTransaction
  in serialOption f "globalTransaction"

globalPosting :: ( OptSpec (Ex.Exceptional Error Operand)
                 , OptSpec (Ex.Exceptional Error Operand) )
globalPosting =
  let f = fmap L.unGlobalPosting . Q.globalPosting
  in serialOption f "globalPosting"

filePosting :: ( OptSpec (Ex.Exceptional Error Operand)
               , OptSpec (Ex.Exceptional Error Operand) )
filePosting =
  let f = fmap L.unFilePosting . Q.filePosting
  in serialOption f "filePosting"

fileTransaction :: ( OptSpec (Ex.Exceptional Error Operand)
                   , OptSpec (Ex.Exceptional Error Operand) )
fileTransaction =
  let f = fmap L.unFileTransaction . Q.fileTransaction
  in serialOption f "fileTransaction"

-- | All operand OptSpec.
operandSpecs
  :: L.DateTime
  -> [OptSpec (CaseSensitive
               -> MatcherFactory
               -> Ex.Exceptional Error Operand)]

operandSpecs dt =
  [ fmap (const . const) date
  , fmap (const . const . pure) (current dt)
  , account
  , accountLevel
  , accountAny
  , payee
  , tag
  , number
  , flag
  , commodity
  , postingMemo
  , transactionMemo
  , filename
  , fmap (const . const . pure) debit
  , fmap (const . const . pure) credit
  , fmap (const . const) qtyOption

  , sAccount
  , sAccountLevel
  , sAccountAny
  , sPayee
  , sTag
  , sNumber
  , sFlag
  , sCommodity
  , sPostingMemo
  , fmap (const . const . pure) sDebit
  , fmap (const . const. pure) sCredit
  , fmap (const . const) sQtyOption
  ]
  ++ serialSpecs

serialSpecs :: [OptSpec (CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional Error Operand)]
serialSpecs
  = concat
  $ [unDouble]
  <*> [ globalTransaction, globalPosting,
        filePosting, fileTransaction,
        sGlobalPosting, sFilePosting,
        sGlobalTransaction, sFileTransaction ]

unDouble
  :: Functor f
  => (f (Ex.Exceptional Error a),
      f (Ex.Exceptional Error a ))
  -> [ f (x -> y -> Ex.Exceptional Error a) ]
unDouble (o1, o2) = [fmap (const . const) o1, fmap (const . const) o2]


------------------------------------------------------------
-- Post filters
------------------------------------------------------------

-- | The user passed a bad number for the head or tail option. The
-- argument is the bad number passed.
data BadHeadTailError = BadHeadTailError Text
  deriving Show

optHead :: OptSpec (Ex.Exceptional Error PostFilterFn)
optHead = C.OptSpec ["head"] [] (C.OneArg f)
  where
    f a = do
      num <- parseInt a
      let g _ ii = ii < (ItemIndex num)
      return g

optTail :: OptSpec (Ex.Exceptional Error PostFilterFn)
optTail = C.OptSpec ["tail"] [] (C.OneArg f)
  where
    f a = do
      num <- parseInt a
      let g (ListLength len) (ItemIndex ii) = ii >= len - num
      return g

postFilterSpecs
  :: ( OptSpec (Ex.Exceptional Error PostFilterFn)
     , OptSpec (Ex.Exceptional Error PostFilterFn))
postFilterSpecs = (optHead, optTail)

------------------------------------------------------------
-- Matcher control
------------------------------------------------------------

parseInsensitive :: OptSpec CaseSensitive
parseInsensitive =
  C.OptSpec ["case-insensitive"] ['i'] (C.NoArg Insensitive)


parseSensitive :: OptSpec CaseSensitive
parseSensitive =
  C.OptSpec ["case-sensitive"] ['I'] (C.NoArg Sensitive)


within :: OptSpec MatcherFactory
within =
  C.OptSpec ["within"] "w" . C.NoArg $ \c t ->
    return (TM.within c t)

pcre :: OptSpec MatcherFactory
pcre = C.OptSpec ["pcre"] "r"
  (C.NoArg (\cs x -> Ex.fromEither $ TM.pcre cs x))

exact :: OptSpec MatcherFactory
exact = C.OptSpec ["exact"] "x" . C.NoArg $ \c t ->
        return (TM.exact c t)

matcherSelectSpecs :: [OptSpec MatcherFactory]
matcherSelectSpecs = [within, pcre, exact]

caseSelectSpecs :: [OptSpec CaseSensitive]
caseSelectSpecs = [parseInsensitive, parseSensitive]

------------------------------------------------------------
-- Operators
------------------------------------------------------------

-- | Open parentheses
open :: OptSpec (X.Token a)
open = C.OptSpec ["open"] "(" (C.NoArg X.openParen)

-- | Close parentheses
close :: OptSpec (X.Token a)
close = C.OptSpec ["close"] ")" (C.NoArg X.closeParen)

-- | and operator
parseAnd :: OptSpec (X.Token a)
parseAnd = C.OptSpec ["and"] "A" (C.NoArg X.opAnd)

-- | or operator
parseOr :: OptSpec (X.Token a)
parseOr = C.OptSpec ["or"] "O" (C.NoArg X.opOr)

-- | not operator
parseNot :: OptSpec (X.Token a)
parseNot = C.OptSpec ["not"] "N" (C.NoArg X.opNot)

operatorSpecs :: [OptSpec (X.Token a)]
operatorSpecs =
  [open, close, parseAnd, parseOr, parseNot]

-- Infix and RPN expression selectors

parseInfix :: OptSpec X.ExprDesc
parseInfix = C.OptSpec ["infix"] "" (C.NoArg X.Infix)

parseRPN :: OptSpec X.ExprDesc
parseRPN = C.OptSpec ["rpn"] "" (C.NoArg X.RPN)

-- | Both Infix and RPN options.
exprDesc :: [OptSpec X.ExprDesc]
exprDesc = [ parseInfix, parseRPN ]

showExpression :: OptSpec ()
showExpression = C.OptSpec ["show-expression"] "" (C.NoArg ())

verboseFilter :: OptSpec ()
verboseFilter = C.OptSpec ["verbose-filter"] "" (C.NoArg ())

--
-- Siblings
--

sGlobalPosting :: ( OptSpec (Ex.Exceptional Error Operand)
                  , OptSpec (Ex.Exceptional Error Operand) )
sGlobalPosting =
  siblingSerialOption "globalPosting"
                      PS.fwdGlobalPosting PS.backGlobalPosting

sFilePosting :: ( OptSpec (Ex.Exceptional Error Operand)
                  , OptSpec (Ex.Exceptional Error Operand) )
sFilePosting =
  siblingSerialOption "filePosting"
                      PS.fwdFilePosting PS.backFilePosting

sGlobalTransaction :: ( OptSpec (Ex.Exceptional Error Operand)
                  , OptSpec (Ex.Exceptional Error Operand) )
sGlobalTransaction =
  siblingSerialOption "globalTransaction"
                      PS.fwdGlobalTransaction PS.backGlobalTransaction

sFileTransaction :: ( OptSpec (Ex.Exceptional Error Operand)
                  , OptSpec (Ex.Exceptional Error Operand) )
sFileTransaction =
  siblingSerialOption "filePosting"
                      PS.fwdFileTransaction PS.backFileTransaction


sAccount :: OptSpec ( CaseSensitive
                    -> MatcherFactory
                    -> Ex.Exceptional Error Operand )
sAccount = C.OptSpec ["s-account"] "" (C.OneArg f)
  where
    f a1 cs fty = fmap PS.account
                  $ getMatcher a1 cs fty

sAccountLevel :: OptSpec ( CaseSensitive
                         -> MatcherFactory
                         -> Ex.Exceptional Error Operand )
sAccountLevel = C.OptSpec ["s-account-level"] "" (C.TwoArg f)
  where
    f a1 a2 cs fty
      = PS.accountLevel <$> parseInt a1 <*> getMatcher a2 cs fty

sAccountAny :: OptSpec ( CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional Error Operand )
sAccountAny = patternOption "s-account-any" Nothing PS.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
sPayee :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional Error Operand )
sPayee = patternOption "s-payee" (Just 'p') PS.payee

sTag :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional Error Operand)
sTag = patternOption "s-tag" (Just 't') PS.tag

sNumber :: OptSpec ( CaseSensitive
                    -> MatcherFactory
                    -> Ex.Exceptional Error Operand )
sNumber = patternOption "s-number" Nothing PS.number

sFlag :: OptSpec ( CaseSensitive
                  -> MatcherFactory
                  -> Ex.Exceptional Error Operand)
sFlag = patternOption "s-flag" Nothing PS.flag

sCommodity :: OptSpec ( CaseSensitive
                       -> MatcherFactory
                       -> Ex.Exceptional Error Operand)
sCommodity = patternOption "s-commodity" Nothing PS.commodity

sPostingMemo :: OptSpec ( CaseSensitive
                         -> MatcherFactory
                         -> Ex.Exceptional Error Operand)
sPostingMemo = patternOption "s-posting-memo" Nothing PS.postingMemo

sDebit :: OptSpec Operand
sDebit = C.OptSpec ["s-debit"] [] (C.NoArg PS.debit)

sCredit :: OptSpec Operand
sCredit = C.OptSpec ["s-credit"] [] (C.NoArg PS.credit)

sQtyOption :: OptSpec (Ex.Exceptional Error Operand)
sQtyOption = C.OptSpec ["s-qty"] [] (C.TwoArg f)
  where
    f a1 a2 = do
      qt <- parseQty a2
      parseComparer a1 (flip PS.qty qt)
    parseQty a = case parse Pc.unquotedQtyRepWithSpaces "" (pack a) of
      Left _ -> Ex.throw "could not parse quantity"
      Right g -> pure . L.toQty $ g

--
-- Versions
--

-- | Prints the binary's version and the version of the library, and exits successfully.

version
  :: V.Version
  -> String
  -- ^ Program name
  -> String
version v pn = unlines
  [ pn ++ " version " ++ V.showVersion v
#ifdef incabal
  , "using version " ++ V.showVersion PPL.version
#else
  , "using testing version"
#endif
    ++ " of penny-lib"
  ]

-- | An option for where the user would like to send output.
output :: MA.OptSpec (X.Text -> IO ())
output = MA.OptSpec ["output"] "o" . MA.OneArg $ \s ->
  if s == "-"
    then TIO.putStr
    else TIO.writeFile s


-- | Given a list of output options, returns a single IO action to
-- write to all given files. If the list was empty, returns an IO
-- action that writes to standard output.

processOutput :: [X.Text -> IO ()] -> X.Text -> IO ()
processOutput ls x =
  if null ls
  then TIO.putStr x
  else sequence_ . map ($ x) $ ls

