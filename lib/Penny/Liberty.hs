{-# LANGUAGE OverloadedStrings #-}

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

  -- * Errors
  Error

  ) where

import Control.Applicative ((<*>), (<$>), pure, Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Text (Text, pack)
import qualified Data.Time as Time
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Combinator (OptSpec)
import Text.Parsec (parse)

import qualified Penny.Copper.Parsec as Pc

import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Predicates as P
import qualified Data.Prednote.Pdct as E
import qualified Penny.Lincoln as L
import qualified System.Console.Rainbow as C
import qualified Data.Prednote.Expressions as X

import Text.Matchers (
  CaseSensitive(Sensitive, Insensitive))
import qualified Text.Matchers as TM

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
  _ -> X.parseExpression d ls

-- | Takes a list of transactions, splits them into PostingChild
-- instances, filters them, post-filters them, sorts them, and places
-- them in Box instances with Filtered serials. Also returns a Text
-- containing a description of the evalutation process.
xactionsToFiltered ::

  P.LPdct
  -- ^ The predicate to filter the transactions

  -> [PostFilterFn]
  -- ^ Post filter specs

  -> (L.PostFam -> L.PostFam -> Ordering)
  -- ^ The sorter

  -> [L.Transaction]
  -- ^ The transactions to work on (probably parsed in from Copper)

  -> ([C.Chunk], [L.Box LibertyMeta])
  -- ^ Sorted, filtered postings

xactionsToFiltered pdct postFilts s txns =
  let pdcts = map (makeLabeledPdct pdct) pfs
      evaluator subj pd = E.evaluate indentAmt True subj 0 pd
      pairMaybes = zipWith evaluator pfs pdcts
      pairs = mapMaybe rmMaybe pairMaybes
      rmMaybe (mayB, x) = case mayB of
        Nothing -> Nothing
        Just b -> Just (b, x)
      pfs = concatMap L.postFam txns
      txt = concatMap snd pairs
      filtered = map snd . filter fst $ zipWith zipper pairs pfs
      zipper (bool, _) pf = (bool, pf)
      resultLs = addSortedNum
                 . processPostFilters postFilts
                 . sortBy (sorter s)
                 . addFilteredNum
                 . map toBox
                 $ filtered
  in (txt, resultLs)


-- | Creates a Pdct and prepends a one-line description of the PostFam
-- to the Pdct's label so it can be easily identified in the output.
makeLabeledPdct :: E.Pdct L.PostFam -> L.PostFam -> E.Pdct L.PostFam
makeLabeledPdct pd pf = E.rename f pd
  where
    f old = old <> " - " <> L.display pf

indentAmt :: E.IndentAmt
indentAmt = 4

-- | Transforms a PostingChild into a Box.
toBox :: L.PostFam -> L.Box ()
toBox = L.Box ()

-- | Takes a list of filtered boxes and adds the Filtered serials.

addFilteredNum :: [L.Box a] -> [L.Box FilteredNum]
addFilteredNum = L.serialItems f where
  f ser = fmap (const (FilteredNum ser))

-- | Wraps a PostingChild sorter to change it to a Box sorter.
sorter :: (L.PostFam -> L.PostFam -> Ordering)
          -> L.Box a
          -> L.Box b
          -> Ordering
sorter f b1 b2 = f (L.boxPostFam b1) (L.boxPostFam b2)

-- | Takes a list of Boxes with metadata and adds a Serial for the
-- Sorted.
addSortedNum ::
  [L.Box FilteredNum]
  -> [L.Box LibertyMeta]
addSortedNum = L.serialItems f where
  f ser = fmap g where
    g filtNum = LibertyMeta filtNum (SortedNum ser)

type MatcherFactory =
  CaseSensitive
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
getMatcher ::
  String
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
parseComparer :: String -> Ex.Exceptional Error P.Comp
parseComparer t
  | t == "<" = return P.DLT
  | t == "<=" = return P.DLTEQ
  | t == "==" = return P.DEQ
  | t == "=" = return P.DEQ
  | t == ">" = return P.DGT
  | t == ">=" = return P.DGTEQ
  | t == "/=" = return P.DNE
  | t == "!=" = return P.DNE
  | otherwise = Ex.throw msg
  where
    msg = "bad comparer: " <> pack t <> "\n"

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

type Operand = E.Pdct L.PostFam

-- | OptSpec for a date.
date :: OptSpec (Ex.Exceptional Error Operand)
date = C.OptSpec ["date"] ['d'] (C.TwoArg f)
  where
    f a1 a2 = P.date <$> parseComparer a1 <*> parseDate a2


current :: L.DateTime -> OptSpec Operand
current dt = C.OptSpec ["current"] [] (C.NoArg f)
  where
    f = P.date P.DLTEQ (L.toUTC dt)

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
number = patternOption "number" Nothing P.number

flag :: OptSpec ( CaseSensitive
                  -> MatcherFactory
                  -> Ex.Exceptional Error Operand)
flag = patternOption "flag" Nothing P.flag

commodity :: OptSpec ( CaseSensitive
                       -> MatcherFactory
                       -> Ex.Exceptional Error Operand)
commodity = patternOption "commodity" Nothing P.commodity

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
qtyOption = C.OptSpec ["qty"] [] (C.TwoArg f)
  where
    f a1 a2 = P.qty <$> parseComparer a1 <*> parseQty a2
    parseQty a = case parse Pc.quantity "" (pack a) of
      Left e -> Ex.throw $ "could not parse quantity: "
        <> pack a <> " - "
        <> (pack . show $ e)
      Right g -> pure g



-- | Creates two options suitable for comparison of serial numbers,
-- one for ascending, one for descending.
serialOption ::

  (L.PostFam -> Maybe L.Serial)
  -- ^ Function that, when applied to a PostingChild, returns the serial
  -- you are interested in.

  -> String
  -- ^ Name of the command line option, such as @global-transaction@

  -> ( OptSpec (Ex.Exceptional Error Operand)
     , OptSpec (Ex.Exceptional Error Operand) )
  -- ^ Parses both descending and ascending serial options.

serialOption getSerial n = (osA, osD)
  where
    osA = C.OptSpec [n] []
          (C.TwoArg (f L.forward))
    osD = C.OptSpec [addPrefix "rev" n] []
          (C.TwoArg (f L.backward))
    f getInt a1 a2 = do
      cmp <- parseComparer a1
      num <- parseInt a2
      let op pf = case getSerial pf of
            Nothing -> False
            Just ser -> getInt ser `cmpFn` num
          (cmpDesc, cmpFn) = P.descComp cmp
          desc = pack n <> " is " <> cmpDesc <> " " <> (pack . show $ num)
      return (E.operand desc op)


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
  let f = fmap L.unGlobalTransaction
          . L.tGlobalTransaction
          . parent
          . L.unPostFam
  in serialOption f "globalTransaction"

globalPosting :: ( OptSpec (Ex.Exceptional Error Operand)
                 , OptSpec (Ex.Exceptional Error Operand) )
globalPosting =
  let f = fmap L.unGlobalPosting
          . L.pGlobalPosting
          . child
          . L.unPostFam
  in serialOption f "globalPosting"

filePosting :: ( OptSpec (Ex.Exceptional Error Operand)
               , OptSpec (Ex.Exceptional Error Operand) )
filePosting =
  let f = fmap L.unFilePosting
          . L.pFilePosting
          . child
          . L.unPostFam
  in serialOption f "filePosting"

fileTransaction :: ( OptSpec (Ex.Exceptional Error Operand)
                   , OptSpec (Ex.Exceptional Error Operand) )
fileTransaction =
  let f = fmap L.unFileTransaction
          . L.tFileTransaction
          . parent
          . L.unPostFam
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
  , fmap (const . const . pure) debit
  , fmap (const . const . pure) credit
  , fmap (const . const) qtyOption
  ]
  ++ serialSpecs

serialSpecs :: [OptSpec (CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional Error Operand)]
serialSpecs
  = concat
  $ [unDouble]
  <*> [ globalTransaction, globalPosting,
        filePosting, fileTransaction ]

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


postFilterSpecs :: ( OptSpec (Ex.Exceptional Error PostFilterFn)
                   , OptSpec (Ex.Exceptional Error PostFilterFn) )
postFilterSpecs = (optHead, optTail)

------------------------------------------------------------
-- Matcher control
------------------------------------------------------------

noArg :: a -> String -> OptSpec a
noArg a s = C.OptSpec [s] "" (C.NoArg a)

parseInsensitive :: OptSpec CaseSensitive
parseInsensitive =
  C.OptSpec ["case-insensitive"] ['i'] (C.NoArg Insensitive)


parseSensitive :: OptSpec CaseSensitive
parseSensitive =
  C.OptSpec ["case-sensitive"] ['I'] (C.NoArg Sensitive)


within :: OptSpec MatcherFactory
within = noArg (\c t -> return (TM.within c t)) "within"

pcre :: OptSpec MatcherFactory
pcre = noArg TM.pcre "pcre"

posix :: OptSpec MatcherFactory
posix = noArg TM.tdfa "posix"

exact :: OptSpec MatcherFactory
exact = noArg (\c t -> return (TM.exact c t)) "exact"

matcherSelectSpecs :: [OptSpec MatcherFactory]
matcherSelectSpecs = [within, pcre, posix, exact]

caseSelectSpecs :: [OptSpec CaseSensitive]
caseSelectSpecs = [parseInsensitive, parseSensitive]

------------------------------------------------------------
-- Operators
------------------------------------------------------------

-- | Open parentheses
open :: OptSpec (X.Token a)
open = noArg X.openParen "open"

-- | Close parentheses
close :: OptSpec (X.Token a)
close = noArg X.closeParen "close"

-- | and operator
parseAnd :: OptSpec (X.Token a)
parseAnd = noArg X.opAnd "and"

-- | or operator
parseOr :: OptSpec (X.Token a)
parseOr = noArg X.opOr "or"

-- | not operator
parseNot :: OptSpec (X.Token a)
parseNot = noArg X.opNot "not"

operatorSpecs :: [OptSpec (X.Token a)]
operatorSpecs =
  [open, close, parseAnd, parseOr, parseNot]

-- Infix and RPN expression selectors

parseInfix :: OptSpec X.ExprDesc
parseInfix = noArg X.Infix "infix"

parseRPN :: OptSpec X.ExprDesc
parseRPN = noArg X.RPN "rpn"

-- | Both Infix and RPN options.
exprDesc :: [OptSpec X.ExprDesc]
exprDesc = [ parseInfix, parseRPN ]

showExpression :: OptSpec ()
showExpression = noArg () "show-expression"

verboseFilter :: OptSpec ()
verboseFilter = noArg () "verbose-filter"

