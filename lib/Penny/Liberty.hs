-- | Liberty - Penny command line parsing utilities
--
-- Both Cabin and Zinc share various functions that aid in parsing
-- command lines. For instance both the Postings report and the Zinc
-- postings filter use common command-line options. However, Zinc
-- already depends on Cabin. To avoid a cyclic dependency whereby
-- Cabin would also depend on Zinc, functions formerly in Zinc that
-- Cabin will also find useful are relocated here, to Liberty.

module Penny.Liberty (
  Error(..),
  MatcherFactory,
  X.evaluate,
  X.Token,
  FilteredNum(FilteredNum, unFilteredNum),
  SortedNum(SortedNum, unSortedNum),
  LibertyMeta(filteredNum, sortedNum),
  xactionsToFiltered,
  ListLength(ListLength, unListLength),
  ItemIndex(ItemIndex, unItemIndex),
  PostFilterFn,
  parseComparer,
  processPostFilters,
  parseTokenList,
  parsePredicate,
  
  -- * Parsers
  Operand,
  operandSpecs,
  postFilterSpecs,
  matcherSelectSpecs,
  caseSelectSpecs,
  operatorSpecs,
  Orderer,
  sortSpecs
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toUpper)
import Data.List (isPrefixOf, sortBy)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Combinator (OptSpec)
import Text.Parsec (parse)
  
import Penny.Copper.DateTime (DefaultTimeZone, dateTime)
import Penny.Copper.Qty (RadGroup, qty)

import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty.Expressions as X

import Text.Matchers.CaseSensitive (
  CaseSensitive(Sensitive, Insensitive))
import qualified Text.Matchers.Text as TM

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


-- | Parses a list of tokens. Returns Nothing if the token sequence is
-- invalid (e.g. if two operators are next to each other) or if the
-- resulting RPN expression is bad (there are tokens left on the stack
-- after parsing). Otherwise, returns the expression.
--
-- An empty list will fail to parse. The function calling this one
-- must deal with empty lists if those are a possibility.
parseTokenList :: [X.Token a] -> Maybe a
parseTokenList = X.evaluate

-- | Parses a list of tokens to obtain a predicate. Deals with an
-- empty list of tokens by returning a predicate that is always
-- True. Fails if the list of tokens is not empty and the parse fails.
parsePredicate :: [X.Token (a -> Bool)] -> Maybe (a -> Bool)
parsePredicate ls = case ls of
  [] -> return (const True)
  ts -> parseTokenList ts

-- | Takes a list of transactions, splits them into PostingChild
-- instances, filters them, post-filters them, sorts them, and places
-- them in Box instances with Filtered serials.
xactionsToFiltered ::
  
  (L.PostFam -> Bool)
  -- ^ The predicate to filter the transactions

  -> [PostFilterFn]
  -- ^ Post filter specs

  -> (L.PostFam -> L.PostFam -> Ordering)
  -- ^ The sorter

  -> [L.Transaction]
  -- ^ The transactions to work on (probably parsed in from Copper)
  
  -> [L.Box LibertyMeta]
  -- ^ Sorted, filtered postings

xactionsToFiltered pdct pfs s =
  addSortedNum
  . processPostFilters pfs
  . sortBy (sorter s)
  . addFilteredNum
  . map toBox
  . filter pdct
  . concatMap L.postFam


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
  -> Ex.Exceptional Text (Text -> Bool)
type Operand = X.Operand (L.PostFam -> Bool)

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
  

data Error = MakeMatcherFactoryError Text
             | DateParseError
             | BadPatternError Text
             | BadNumberError Text
             | BadQtyError Text
             | BadSortKeyError Text
             | BadComparator Text
             | BadExpression
             | BadColorName Text
             | BadFieldName Text
             | BadBackgroundArg Text
             | UnexpectedWord Text Text
             | BadCommodityError Text
             deriving Show


------------------------------------------------------------
-- Operands
------------------------------------------------------------

-- | Given a String from the command line which represents a pattern,
-- and a MatcherFactor, return a Matcher. Fails if the pattern is bad
-- (e.g. it is not a valid regular expression).
getMatcher ::
  String
  -> CaseSensitive
  -> MatcherFactory
  -> (Text -> Bool)

getMatcher s cs f = Ex.resolve e $ f cs (pack s)
  where
    e err = abort $ "bad pattern: " ++ s ++ " error message: "
            ++ unpack err

-- | Parses comparers given on command line to a function. Aborts if
-- the string given is invalid.
parseComparer ::
  (Eq a, Ord a)
  => String
  -> (a -> a -> Bool)
parseComparer t
  | t == "<" = (<)
  | t == "<=" = (<=)
  | t == "==" = (==)
  | t == "=" = (==)
  | t == ">" = (>)
  | t == ">=" = (>=)
  | t == "/=" = (/=)
  | t == "!=" = (/=)
  | otherwise = abort $ "invalid comparer: " ++ t

parseDate :: DefaultTimeZone -> Text -> L.DateTime
parseDate dtz t = case parse (dateTime dtz) "" t of
  Left _ -> abort $ "could not parse date: " ++ unpack t
  Right d -> d

date :: OptSpec (DefaultTimeZone -> Operand)
date = C.OptSpec ["date"] ['d'] (C.TwoArg f)
  where
    f a1 a2 dtz =
      let cmp = parseComparer a1
          dt = parseDate dtz (pack a2)
      in X.Operand (P.date (`cmp` dt))


current :: OptSpec (L.DateTime -> Operand)
current = C.OptSpec ["current"] [] (C.NoArg f)
  where
    f dt = X.Operand (P.date (<= dt))


-- | Creates options that match against fields that are
-- colon-separated. The operand added will return True if the
-- posting's colon-separated option matches the pattern given, or
-- False if it does not.
sepOption ::
  String
  -- ^ Long option name
  
  -> Maybe Char
  -- ^ Short option name, if there is one

  -> (Text -> (Text -> Bool) -> L.PostFam -> Bool)
  -- ^ When applied to a text that separates the different segments of
  -- the field, a matcher, and a posting, this function returns True
  -- if the posting matches the matcher or False if it does not.
  
  -> OptSpec (CaseSensitive -> MatcherFactory -> Operand)
sepOption str mc f = C.OptSpec [str] so (C.OneArg g)
  where
    so = case mc of
      Nothing -> []
      Just c -> [c]
    g a cs fty =
      let m = getMatcher a cs fty
      in X.Operand (f sep m)


sep :: Text
sep = Text.singleton ':'

-- | Parses exactly one integer; fails if it cannot read exactly one.
parseInt :: String -> Int
parseInt t =
  let ps = reads t
      err = abort $ "invalid number: " ++ t
  in case ps of
    ((i, s):[]) -> if length s /= 0 then err else i
    _ -> err

-- | Creates options that take two arguments, with the first argument
-- being the level to match, and the second being the pattern that
-- should match against that level.  Adds an operand that returns True
-- if the pattern matches.
levelOption ::
  String
  -- ^ Long option name
  
  -> (Int -> (Text -> Bool) -> L.PostFam -> Bool)
  -- ^ Applied to an integer, a matcher, and a PostingBox, this
  -- function returns True if a particular field in the posting
  -- matches the matcher given at the given level, or False otherwise.
  
  -> OptSpec (CaseSensitive -> MatcherFactory -> Operand)
  
levelOption str f = C.OptSpec [str] [] (C.TwoArg g)
  where
      g a1 a2 cs fty =
        let n = parseInt a1
            m = getMatcher a2 cs fty
        in X.Operand (f n m)


-- | Creates options that add an operand that matches the posting if a
-- particluar field matches the pattern given.
patternOption ::
  String
  -- ^ Long option
  
  -> Maybe Char
  -- ^ Short option, if included

  -> ((Text -> Bool) -> L.PostFam -> Bool)
  -- ^ When applied to a matcher and a PostingBox, this function
  -- returns True if the posting matches, or False if it does not.
  
  -> OptSpec (CaseSensitive -> MatcherFactory -> Operand)
patternOption str mc f = C.OptSpec [str] so (C.OneArg g)
  where
    so = maybe [] (\c -> [c]) mc
    g a1 cs fty =
      let m = getMatcher a1 cs fty
      in X.Operand (f m)


-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
account = sepOption "account" (Just 'a') P.account

-- | The account-level option; matches if the account at the given
-- level matches.
accountLevel :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
accountLevel = levelOption "account-level" P.accountLevel

-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
payee = patternOption "payee" (Just 'p') P.payee

tag :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)       
tag = patternOption "tag" (Just 't') P.tag

number :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
number = patternOption "number" Nothing P.number

flag :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
flag = patternOption "flag" Nothing P.flag

commodity :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
commodity = sepOption "commodity" Nothing P.commodity

commodityLevel :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
commodityLevel = levelOption "commodity-level" P.commodityLevel

commodityAny :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
commodityAny = patternOption "commodity" Nothing P.commodityAny

postingMemo :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: OptSpec (CaseSensitive -> MatcherFactory -> Operand)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

debit :: OptSpec Operand
debit = C.OptSpec ["debit"] [] (C.NoArg (X.Operand P.debit))

credit :: OptSpec Operand
credit = C.OptSpec ["credit"] [] (C.NoArg (X.Operand P.credit))

qtyOption :: OptSpec (RadGroup -> Operand)
qtyOption = C.OptSpec ["qty"] [] (C.TwoArg f)
  where
    f a1 a2 rg =
      let q = case parse (qty rg) "" (pack a2) of
            Left _ -> abort $ "invalid quantity: " ++ a2
            Right g -> g
          cmp = parseComparer a1
      in X.Operand (P.qty (`cmp` q))

-- | Creates two options suitable for comparison of serial numbers,
-- one for ascending, one for descending.
serialOption ::

  (L.PostFam -> Maybe L.Serial)
  -- ^ Function that, when applied to a PostingChild, returns the serial
  -- you are interested in.

  -> String
  -- ^ Name of the command line option, such as @global-transaction@

  -> (OptSpec Operand, OptSpec Operand)
  -- ^ Parses both descending and ascending serial options.

serialOption getSerial n = (osA, osD)
  where
    osA = C.OptSpec [n] []
          (C.TwoArg (f L.forward))
    osD = C.OptSpec [addPrefix "rev" n] []
          (C.TwoArg (f L.backward))
    f getInt a1 a2 =
      let cmp = parseComparer a1
          i = parseInt a2
          op pf = case getSerial pf of
            Nothing -> False
            Just ser -> getInt ser `cmp` i
      in X.Operand op


-- | Takes a string, adds a prefix and capitalizes the first letter of
-- the old string. e.g. applied to "rev" and "globalTransaction",
-- returns "revGlobalTransaction".
addPrefix :: String -> String -> String
addPrefix pre suf = pre ++ suf' where
  suf' = case suf of
    "" -> ""
    x:xs -> toUpper x : xs

globalTransaction :: (OptSpec Operand, OptSpec Operand)
globalTransaction =
  let f = fmap L.unGlobalTransaction
          . L.globalTransaction
          . L.tMeta
          . parent
          . L.unPostFam
  in serialOption f "globalTransaction"

globalPosting :: (OptSpec Operand, OptSpec Operand)
globalPosting =
  let f = fmap L.unGlobalPosting
          . L.globalPosting
          . L.pMeta
          . child
          . L.unPostFam
  in serialOption f "globalPosting"

filePosting :: (OptSpec Operand, OptSpec Operand)
filePosting =
  let f = fmap L.unFilePosting
          . L.filePosting
          . L.pMeta
          . child
          . L.unPostFam
  in serialOption f "filePosting"

fileTransaction :: (OptSpec Operand, OptSpec Operand)
fileTransaction =
  let f = fmap L.unFileTransaction
          . L.fileTransaction
          . L.tMeta
          . parent
          . L.unPostFam
  in serialOption f "fileTransaction"

unDouble :: (OptSpec Operand, OptSpec Operand)
            -> [OptSpec (a -> b -> c -> d -> e -> Operand)]
unDouble (o1, o2) = [fmap f o1, fmap f o2]
  where
    f = (\g _ _ _ _ _ -> g)


-- | All operand OptSpec.
operandSpecs :: [OptSpec (L.DateTime
                          -> DefaultTimeZone
                          -> RadGroup
                          -> CaseSensitive
                          -> MatcherFactory
                          -> Operand)]

operandSpecs =
  [ fmap (\f _ dtz _ _ _ -> f dtz) date
  , fmap (\f dt _ _ _ _ -> f dt) current
  , wrapFactArg account
  , wrapFactArg accountLevel
  , wrapFactArg accountAny
  , wrapFactArg payee
  , wrapFactArg tag
  , wrapFactArg number
  , wrapFactArg flag
  , wrapFactArg commodity
  , wrapFactArg commodityLevel
  , wrapFactArg commodityAny
  , wrapFactArg postingMemo
  , wrapFactArg transactionMemo
  , fmap (\f _ _ _ _ _ -> f) debit
  , fmap (\f _ _ _ _ _  -> f) credit
  , fmap (\f _ _ rg _ _ -> f rg) qtyOption
  ]
  ++ unDouble globalTransaction
  ++ unDouble globalPosting
  ++ unDouble filePosting
  ++ unDouble fileTransaction
    where
      wrapFactArg = fmap (\f _ _ _ cs fty -> f cs fty)

------------------------------------------------------------
-- Post filters
------------------------------------------------------------

optHead :: OptSpec PostFilterFn
optHead = C.OptSpec ["head"] [] (C.OneArg f)
  where
    f a _ ii =
      let i = ItemIndex . parseInt $ a
      in ii < i

optTail :: OptSpec PostFilterFn
optTail = C.OptSpec ["tail"] [] (C.OneArg f)
  where
    f a (ListLength len) (ItemIndex ii) =
      let i = parseInt a
      in ii >= len - i

postFilterSpecs :: (OptSpec PostFilterFn, OptSpec PostFilterFn)
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
open = noArg X.TokOpenParen "open"

-- | Close parentheses
close :: OptSpec (X.Token a)
close = noArg X.TokCloseParen "close"

-- | and operator
parseAnd :: OptSpec (X.Token (a -> Bool))
parseAnd = noArg X.tokAnd "and"

-- | or operator
parseOr :: OptSpec (X.Token (a -> Bool))
parseOr = noArg X.tokOr "or"

-- | not operator
parseNot :: OptSpec (X.Token (a -> Bool))
parseNot = noArg X.tokNot "not" where

operatorSpecs :: [OptSpec (X.Token (a -> Bool))]
operatorSpecs =
  [open, close, parseAnd, parseOr, parseNot]

------------------------------------------------------------
-- Sorting
------------------------------------------------------------
type Orderer = L.PostFam -> L.PostFam -> Ordering

ordering ::
  (Ord b)
  => (a -> b)
  -> (a -> a -> Ordering)
ordering q = f where
  f p1 p2 = compare (q p1) (q p2)


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
  [ ("payee", ordering Q.payee)
  , ("date", ordering Q.dateTime)
  , ("flag", ordering Q.flag)
  , ("number", ordering Q.number)
  , ("account", ordering Q.account)
  , ("drCr", ordering Q.drCr)
  , ("qty", ordering Q.qty)
  , ("commodity", ordering Q.commodity)
  , ("postingMemo", ordering Q.postingMemo)
  , ("transactionMemo", ordering Q.transactionMemo) ]

ords :: [(String, Orderer)]
ords = ordPairs ++ uppers where
  uppers = map toReversed ordPairs
  toReversed (s, f) =
    (capitalizeFirstLetter s, flipOrder f)


sortSpecs :: OptSpec Orderer
sortSpecs = C.OptSpec ["sort"] ['s'] (C.OneArg f)
  where
    f a =
      let matches = filter (\p -> a `isPrefixOf` (fst p)) ords
      in case matches of
        x:[] -> snd x
        _ -> abort $ "invalid sort key: " ++ a


abort :: String -> a
abort s = error $ "error: " ++ s
