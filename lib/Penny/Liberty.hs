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
  OperandError(..),
  BadHeadTailError(..)

  ) where

import Control.Applicative ((<*>), (<$>), pure, Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Text (Text, pack)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Combinator (OptSpec)
import Text.Parsec (parse)

import qualified Penny.Copper.Parsec as Pc

import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Steel.Pdct as E
import qualified Penny.Lincoln as L
import qualified Penny.Steel.Chunk as C
import qualified Penny.Steel.Error as Er
import qualified Penny.Steel.Expressions as X

import Text.Matchers (
  CaseSensitive(Sensitive, Insensitive))
import qualified Text.Matchers as TM

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
  -> Ex.Exceptional (X.ExprError a) (E.Pdct a)
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
  -> Ex.Exceptional BadPatternError TM.Matcher

getMatcher s cs f
  = Ex.mapException (BadPatternError (pack s))
  $ f cs (pack s)

-- | Bad string given for a comparer.
data BadComparer = BadComparer Text
  deriving Show

-- | Parses comparers given on command line to a function. Fails if
-- the string given is invalid.
parseComparer :: String -> Ex.Exceptional BadComparer P.Comp
parseComparer t
  | t == "<" = return P.DLT
  | t == "<=" = return P.DLTEQ
  | t == "==" = return P.DEQ
  | t == "=" = return P.DEQ
  | t == ">" = return P.DGT
  | t == ">=" = return P.DGTEQ
  | t == "/=" = return P.DNE
  | t == "!=" = return P.DNE
  | otherwise = Ex.throw $ BadComparer (pack t)

-- | Bad date provided. The first argument is the bad string provided;
-- the second argument is the error message.
data BadDate = BadDate Text Text
  deriving Show

-- | Parses a date from the command line. On failure, throws back the
-- error message from the failed parse.
parseDate :: String -> Ex.Exceptional BadDate Time.UTCTime
parseDate arg =
  Ex.mapExceptional (BadDate (pack arg) . pack . show) L.toUTC
  . Ex.fromEither
  . parse Pc.dateTime ""
  . pack
  $ arg

type Operand = E.Pdct L.PostFam

-- | An error when parsing the @--date@ option.
data DateOptError
  = DateOptBadComparer BadComparer
  -- ^ Bad comparer text provided; the argument is the text that was
  -- provided.

  | DateOptBadDate BadDate
  -- ^ Bad date string provided; the first argument is the bad input,
  -- and the second is the error message.
  deriving Show

-- | OptSpec for a date.
date :: OptSpec (Ex.Exceptional (Either BadComparer BadDate) Operand)
date = C.OptSpec ["date"] ['d'] (C.TwoArg f)
  where
    f a1 a2
      = g
      <$> Ex.mapException DateOptBadComparer
          (parseComparer a1)
      <*> Ex.mapException DateOptBadDate
          (parseDate a2)
    g cmp dt = P.date cmp dt


current :: L.DateTime -> OptSpec Operand
current dt = C.OptSpec ["current"] [] (C.NoArg f)
  where
    f = P.date P.DLTEQ (L.toUTC dt)

-- | Parses exactly one integer; fails if it cannot read exactly one.
parseInt :: String -> Maybe Int
parseInt t =
  case reads t of
    ((i, ""):[]) -> return i
    _ -> Nothing


-- | Bad pattern supplied for a regular expression. The first argument
-- is the bad string supplied; the second argument is the error
-- message.
data BadPatternError = BadPatternError Text Text
  deriving Show

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
               -> Ex.Exceptional BadPatternError Operand )
patternOption str mc f = C.OptSpec [str] so (C.OneArg g)
  where
    so = maybe [] (:[]) mc
    g a1 cs fty = f <$> getMatcher a1 cs fty


-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: OptSpec ( CaseSensitive
                   -> MatcherFactory
                   -> Ex.Exceptional BadPatternError Operand )
account = C.OptSpec ["account"] "a" (C.OneArg f)
  where
    f a1 cs fty
      = fmap P.account
      $ getMatcher a1 cs fty


-- | An error when parsing the @--account-level@ option.
data AccountLevelError
  = AccountLevelBadLevel Text
  -- ^ The string provided for the level was bad; for example, it
  -- contained a letter.

  | AccountLevelBadPattern BadPatternError
  -- ^ A bad pattern was given for the account name.

  deriving Show

-- | The account-level option; matches if the account at the given
-- level matches.
accountLevel :: OptSpec ( CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional AccountLevelError Operand)
accountLevel = C.OptSpec ["account-level"] "" (C.TwoArg f)
  where
    f a1 a2 cs fty = do
      lvl <- Ex.fromMaybe (AccountLevelBadLevel (pack a1))
             $ parseInt a1
      mr <- Ex.mapException AccountLevelBadPattern
            $ getMatcher a2 cs fty
      return $ P.accountLevel lvl mr


-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: OptSpec ( CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional BadPatternError Operand )
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional BadPatternError Operand )
payee = patternOption "payee" (Just 'p') P.payee

tag :: OptSpec ( CaseSensitive
                 -> MatcherFactory
                 -> Ex.Exceptional BadPatternError Operand)
tag = patternOption "tag" (Just 't') P.tag

number :: OptSpec ( CaseSensitive
                    -> MatcherFactory
                    -> Ex.Exceptional BadPatternError Operand )
number = patternOption "number" Nothing P.number

flag :: OptSpec ( CaseSensitive
                  -> MatcherFactory
                  -> Ex.Exceptional BadPatternError Operand)
flag = patternOption "flag" Nothing P.flag

commodity :: OptSpec ( CaseSensitive
                       -> MatcherFactory
                       -> Ex.Exceptional BadPatternError Operand)
commodity = patternOption "commodity" Nothing P.commodity

postingMemo :: OptSpec ( CaseSensitive
                         -> MatcherFactory
                         -> Ex.Exceptional BadPatternError Operand)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: OptSpec ( CaseSensitive
                             -> MatcherFactory
                             -> Ex.Exceptional BadPatternError Operand)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

debit :: OptSpec Operand
debit = C.OptSpec ["debit"] [] (C.NoArg P.debit)

credit :: OptSpec Operand
credit = C.OptSpec ["credit"] [] (C.NoArg P.credit)

-- | An error when parsing the @--qty@ option.
data QtyError

  = QtyBadComparer BadComparer
  -- ^ The comparer could not be parsed; the argument is the comparer
  -- the user supplied.

  | QtyBadQty Text Text
  -- ^ The user passed a bad string for the quantity. The first
  -- argument is the bad quantity given; the second is the error
  -- message.
  deriving Show

qtyOption :: OptSpec (Ex.Exceptional QtyError Operand)
qtyOption = C.OptSpec ["qty"] [] (C.TwoArg f)
  where
    f a1 a2 = do
      comp <- Ex.mapException QtyBadComparer
              $ parseComparer a1
      qty <- Ex.mapException (QtyBadQty (pack a2) . pack . show )
             . Ex.fromEither
             $ parse Pc.quantity "" (pack a2)
      return $ P.qty comp qty


-- | An error when parsing one of the options that matches serials.
data BadSerialError
  = BadSerialComp BadComparer
  -- ^ Bad input for comparer; the Text is the bad input

  | BadSerialNumber Text
  -- ^ Bad input for number; the Text is the bad input
  deriving Show

-- | Creates two options suitable for comparison of serial numbers,
-- one for ascending, one for descending.
serialOption ::

  (L.PostFam -> Maybe L.Serial)
  -- ^ Function that, when applied to a PostingChild, returns the serial
  -- you are interested in.

  -> String
  -- ^ Name of the command line option, such as @global-transaction@

  -> ( OptSpec (Ex.Exceptional BadSerialError Operand)
     , OptSpec (Ex.Exceptional BadSerialError Operand) )
  -- ^ Parses both descending and ascending serial options.

serialOption getSerial n = (osA, osD)
  where
    osA = C.OptSpec [n] []
          (C.TwoArg (f L.forward))
    osD = C.OptSpec [addPrefix "rev" n] []
          (C.TwoArg (f L.backward))
    f getInt a1 a2 = do
      cmp <- Ex.mapException BadSerialComp
             $ parseComparer a1
      num <- Ex.fromMaybe (BadSerialNumber . pack $ a2)
             $ parseInt a2
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

globalTransaction :: ( OptSpec (Ex.Exceptional BadSerialError Operand)
                     , OptSpec (Ex.Exceptional BadSerialError Operand) )
globalTransaction =
  let f = fmap L.unGlobalTransaction
          . L.tGlobalTransaction
          . parent
          . L.unPostFam
  in serialOption f "globalTransaction"

globalPosting :: ( OptSpec (Ex.Exceptional BadSerialError Operand)
                 , OptSpec (Ex.Exceptional BadSerialError Operand) )
globalPosting =
  let f = fmap L.unGlobalPosting
          . L.pGlobalPosting
          . child
          . L.unPostFam
  in serialOption f "globalPosting"

filePosting :: ( OptSpec (Ex.Exceptional BadSerialError Operand)
               , OptSpec (Ex.Exceptional BadSerialError Operand) )
filePosting =
  let f = fmap L.unFilePosting
          . L.pFilePosting
          . child
          . L.unPostFam
  in serialOption f "filePosting"

fileTransaction :: ( OptSpec (Ex.Exceptional BadSerialError Operand)
                   , OptSpec (Ex.Exceptional BadSerialError Operand) )
fileTransaction =
  let f = fmap L.unFileTransaction
          . L.tFileTransaction
          . parent
          . L.unPostFam
  in serialOption f "fileTransaction"

-- | Transforms an Exceptional inside a functor to an Exceptional with
-- a different exception that takes two arguments.
lift0Ex
  :: Functor f
  => (e -> b)
  -> f (Ex.Exceptional e a)
  -> f (x -> y -> Ex.Exceptional b a)
lift0Ex convert container = fmap f container
  where
    f ex = \_ _ -> Ex.mapException convert ex

-- | Transforms a value inside a functor to a function that takes two
-- arguments and returns an Applicative.
lift0
  :: (Functor f, Applicative ap)
  => f a
  -> f (x -> y -> ap a)
lift0 = fmap f
  where
    f a = \_ _ -> pure a

-- | Transforms the exception of a function inside a functor that
-- returns an Exceptional.
lift3
  :: Functor f
  => (e1 -> e2)
  -> f (x -> y -> Ex.Exceptional e1 a)
  -> f (x -> y -> Ex.Exceptional e2 a)
lift3 mapE = fmap g
  where
    g f = \x y -> Ex.mapException mapE (f x y)

-- | All operand OptSpec.
operandSpecs
  :: L.DateTime
  -> [OptSpec (CaseSensitive
               -> MatcherFactory
               -> Ex.Exceptional OperandError Operand)]

operandSpecs dt =
  [ lift0Ex OEDateError date
  , lift0 (current dt)
  , lift3 OEBadPatternError account
  , lift3 OEAccountLevelError accountLevel
  , lift3 OEBadPatternError accountAny
  , lift3 OEBadPatternError payee
  , lift3 OEBadPatternError tag
  , lift3 OEBadPatternError number
  , lift3 OEBadPatternError flag
  , lift3 OEBadPatternError commodity
  , lift3 OEBadPatternError postingMemo
  , lift3 OEBadPatternError transactionMemo
  , lift0 debit
  , lift0 credit
  , fmap (\ex -> \_ _ -> Ex.mapException OEQtyError ex) qtyOption
  ]
  ++ serialSpecs

serialSpecs :: [OptSpec (CaseSensitive
                        -> MatcherFactory
                        -> Ex.Exceptional OperandError Operand)]
serialSpecs
  = concat
  $ [unDouble]
  <*> [ globalTransaction, globalPosting,
        filePosting, fileTransaction ]

unDouble
  :: Functor f
  => (f (Ex.Exceptional BadSerialError a),
      f (Ex.Exceptional BadSerialError a ))
  -> [ f (x -> y -> Ex.Exceptional OperandError a) ]
unDouble (o1, o2) = [fmap f o1, fmap f o2]
  where
    f = (\g _ _ -> Ex.mapException OEBadSerialError g)


------------------------------------------------------------
-- Post filters
------------------------------------------------------------

-- | The user passed a bad number for the head or tail option. The
-- argument is the bad number passed.
data BadHeadTailError = BadHeadTailError Text
  deriving Show

optHead :: OptSpec (Ex.Exceptional BadHeadTailError PostFilterFn)
optHead = C.OptSpec ["head"] [] (C.OneArg f)
  where
    f a = do
      num <- Ex.fromMaybe (BadHeadTailError (pack a))
             $ parseInt a
      let g _ ii = ii < (ItemIndex num)
      return g

optTail :: OptSpec (Ex.Exceptional BadHeadTailError PostFilterFn)
optTail = C.OptSpec ["tail"] [] (C.OneArg f)
  where
    f a = do
      num <- Ex.fromMaybe (BadHeadTailError (pack a))
             $ parseInt a
      let g (ListLength len) (ItemIndex ii) = ii >= len - num
      return g


postFilterSpecs :: ( OptSpec (Ex.Exceptional BadHeadTailError PostFilterFn)
                   , OptSpec (Ex.Exceptional BadHeadTailError PostFilterFn) )
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

--
-- Errors
--

-- | An error occurred when processing the operands in a posting
-- filter expression.
data OperandError
  = OEDateError DateOptError
  -- ^ An error when parsing the @--date@ option

  | OEBadPatternError BadPatternError
  -- ^ The user provided a bad pattern for a regular expression.

  | OEAccountLevelError AccountLevelError
  -- ^ An error when parsing the @--account-level@ option.

  | OEQtyError QtyError
  -- ^ An error when parsing the @--qty@ option.

  | OEBadSerialError BadSerialError
  -- ^ An error when parsing one of the options that matches serials.

  deriving Show


