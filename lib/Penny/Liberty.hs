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
  
  -- * Parsers
  Operand,
  parseOperand,
  parsePostFilter,
  parseMatcherSelect,
  parseCaseSelect,
  parseOperator,
  Orderer,
  parseSort
  ) where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception))
import Data.Char (toUpper)
import Data.List (isPrefixOf, sortBy)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)
import Text.Parsec (parse)
  
import Penny.Copper.DateTime (DefaultTimeZone, dateTime)
import Penny.Copper.Qty (RadGroup, qty)

import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty.Queue as Queue
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
parseTokenList :: [X.Token a] -> Maybe a
parseTokenList = X.evaluate . foldl (flip Queue.enqueue) Queue.empty

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
  . concatMap makeChildren


-- | Transform a list of transactions into a list of children.
makeChildren :: L.Transaction -> [L.PostFam]
makeChildren = undefined

-- | Transforms a PostingChild into a Box.
toBox :: L.PostFam -> L.Box ()
toBox = L.Box ()

-- | Takes a list of filtered boxes and adds the Filtered serials.

addFilteredNum :: [L.Box ()] -> [L.Box FilteredNum]
addFilteredNum = undefined

-- | Wraps a PostingChild sorter to change it to a Box sorter.
sorter :: (L.PostFam -> L.PostFam -> Ordering)
          -> L.Box a
          -> L.Box b
          -> Ordering
sorter = undefined

-- | Takes a list of Boxes with metadata and adds a Serial for the
-- Sorted.
addSortedNum ::
  [L.Box FilteredNum]
  -> [L.Box LibertyMeta]
addSortedNum = undefined

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
  -> Ex.Exceptional Error (Text -> Bool)

getMatcher s cs f = case f cs (pack s) of
  Ex.Exception e -> Ex.Exception $ BadPatternError e
  Ex.Success m -> return m

-- | Parses comparers given on command line to a function.
parseComparer ::
  (Eq a, Ord a)
  => String
  -> Maybe (a -> a -> Bool)
parseComparer t
  | t == "<" = Just (<)
  | t == "<=" = Just (<=)
  | t == "==" = Just (==)
  | t == ">" = Just (>)
  | t == ">=" = Just (>=)
  | t == "/=" = Just (/=)
  | t == "!=" = Just (/=)
  | otherwise = Nothing

parseDate :: DefaultTimeZone -> Text -> Exceptional Error L.DateTime
parseDate dtz t = case parse (dateTime dtz) "" t of
  Left _ -> Exception DateParseError
  Right d -> return d

date :: Parser (DefaultTimeZone -> Ex.Exceptional Error Operand)
date =
  let os = C.OptSpec ["date"] ['d'] (C.TwoArg f)
      f a1 a2 dtz = do
        cmp <- Ex.fromMaybe (BadComparator (pack a1))
               (parseComparer a1)
        dt <- parseDate dtz (pack a2)
        return $ X.Operand (P.date (`cmp` dt))
  in C.parseOption [os]

current :: Parser (L.DateTime -> Operand)
current =
  let os = C.OptSpec ["current"] [] (C.NoArg f)
      f dt = X.Operand (P.date (<= dt))
  in C.parseOption [os]

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
  
  -> Parser (CaseSensitive
             -> MatcherFactory
             -> Ex.Exceptional Error Operand)
sepOption str mc f =
  let so = case mc of
        Nothing -> []
        Just c -> [c]
      os = C.OptSpec [str] so (C.OneArg g)
      g a cs fty = do
        m <- getMatcher a cs fty
        return $ X.Operand (f sep m)
  in C.parseOption [os]

sep :: Text
sep = Text.singleton ':'

-- | Parses exactly one integer; fails if it cannot read exactly one.
parseInt :: String -> Exceptional Error Int
parseInt t = let ps = reads t in
  case ps of
    [] -> Exception $ BadNumberError (pack t)
    ((i, s):[]) -> if length s /= 0
                   then Exception $ BadNumberError (pack t)
                   else return i
    _ -> Exception $ BadNumberError (pack t)

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
  
  -> Parser (CaseSensitive
             -> MatcherFactory -> Exceptional Error Operand)
  
levelOption str f =
  let os = C.OptSpec [str] [] (C.TwoArg g)
      g a1 a2 cs fty = do
        n <- parseInt a1
        m <- getMatcher a2 cs fty
        return $ X.Operand (f n m)
  in C.parseOption [os]

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
  
  -> Parser (CaseSensitive
             -> MatcherFactory -> Exceptional Error Operand)
patternOption str mc f =
  let so = maybe [] (\c -> [c]) mc
      os = C.OptSpec [str] so (C.OneArg g)
      g a1 cs fty = do
        m <- getMatcher a1 cs fty
        return $ X.Operand (f m)
  in C.parseOption [os]

-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: Parser (CaseSensitive
                   -> MatcherFactory -> Exceptional Error Operand)
account = sepOption "account" (Just 'a') P.account

-- | The account-level option; matches if the account at the given
-- level matches.
accountLevel :: Parser (CaseSensitive
                        -> MatcherFactory -> Exceptional Error Operand)
accountLevel = levelOption "account-level" P.accountLevel

-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: Parser (CaseSensitive
                      -> MatcherFactory -> Exceptional Error Operand)
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: Parser (CaseSensitive
                 -> MatcherFactory -> Exceptional Error Operand)
payee = patternOption "payee" (Just 'p') P.payee

tag :: Parser (CaseSensitive
               -> MatcherFactory -> Exceptional Error Operand)
tag = patternOption "tag" (Just 't') P.tag

number :: Parser (CaseSensitive
                  -> MatcherFactory -> Exceptional Error Operand)
number = patternOption "number" Nothing P.number

flag :: Parser (CaseSensitive
                -> MatcherFactory -> Exceptional Error Operand)
flag = patternOption "flag" Nothing P.flag

commodity :: Parser (CaseSensitive
                     -> MatcherFactory -> Exceptional Error Operand)
commodity = sepOption "commodity" Nothing P.commodity

commodityLevel :: Parser (CaseSensitive
                          -> MatcherFactory -> Exceptional Error Operand)
commodityLevel = levelOption "commodity-level" P.commodityLevel

commodityAny :: Parser (CaseSensitive
                        -> MatcherFactory -> Exceptional Error Operand)
commodityAny = patternOption "commodity" Nothing P.commodityAny

postingMemo :: Parser (CaseSensitive
                       -> MatcherFactory -> Exceptional Error Operand)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: Parser (CaseSensitive
                           -> MatcherFactory -> Exceptional Error Operand)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

debit :: Parser Operand
debit =
  let os = C.OptSpec ["debit"] [] (C.NoArg (X.Operand P.debit))
  in C.parseOption [os]

credit :: Parser Operand
credit =
  let os = C.OptSpec ["credit"] [] (C.NoArg (X.Operand P.credit))
  in C.parseOption [os]

qtyOption :: Parser (RadGroup -> Exceptional Error Operand)
qtyOption =
  let os = C.OptSpec ["qty"] [] (C.TwoArg f)
      f a1 a2 rg = do
        q <- case parse (qty rg) "" (pack a2) of
          Left _ -> Ex.throw $ BadQtyError (pack a2)
          Right g -> return g
        cmp <- Ex.fromMaybe (BadComparator (pack a1))
               (parseComparer a1)
        return $ X.Operand (P.qty (`cmp` q))
  in C.parseOption [os]

-- | Creates two options suitable for comparison of serial numbers,
-- one for ascending, one for descending.
serialOption ::

  (L.PostFam -> Maybe L.Serial)
  -- ^ Function that, when applied to a PostingChild, returns the serial
  -- you are interested in.

  -> String
  -- ^ Name of the command line option, such as @global-transaction@

  -> Parser (Exceptional Error Operand)
  -- ^ Parses both descending and ascending serial options.

serialOption getSerial n =
  let osA = C.OptSpec [n ++ "-ascending"] []
            (C.TwoArg (f L.forward))
      osD = C.OptSpec [n ++ "-descending"] []
            (C.TwoArg (f L.backward))
      f getInt a1 a2 = do
        cmp <- Ex.fromMaybe (BadComparator (pack a1))
               (parseComparer a1)
        i <- parseInt a2
        let op pf = case getSerial pf of
              Nothing -> False
              Just ser -> getInt ser `cmp` i
        return $ X.Operand op
  in C.parseOption [osA, osD]

globalTransaction :: Parser (Exceptional Error Operand)
globalTransaction =
  let f = fmap L.unGlobalTransaction
          . L.globalTransaction
          . L.tMeta
          . parent
          . L.unPostFam
  in serialOption f "global-transaction"

globalPosting :: Parser (Exceptional Error Operand)
globalPosting =
  let f = fmap L.unGlobalPosting
          . L.globalPosting
          . L.pMeta
          . child
          . L.unPostFam
  in serialOption f "global-posting"

filePosting :: Parser (Exceptional Error Operand)
filePosting =
  let f = fmap L.unFilePosting
          . L.filePosting
          . L.pMeta
          . child
          . L.unPostFam
  in serialOption f "file-posting"

fileTransaction :: Parser (Exceptional Error Operand)
fileTransaction =
  let f = fmap L.unFileTransaction
          . L.fileTransaction
          . L.tMeta
          . parent
          . L.unPostFam
  in serialOption f "file-transaction"

-- | Parses operands.
parseOperand ::
  Parser (L.DateTime
          -> DefaultTimeZone
          -> RadGroup
          -> CaseSensitive
          -> MatcherFactory
          -> Ex.Exceptional Error Operand)

parseOperand =
  (do { f <- date; return (\_ dtz _ _ _ -> f dtz)})
  <|> (do { f <- current; return (\dt _ _ _ _ -> return (f dt)) })
  <|> wrapFactArg account
  <|> wrapFactArg accountLevel
  <|> wrapFactArg accountAny
  <|> wrapFactArg payee
  <|> wrapFactArg tag
  <|> wrapFactArg number
  <|> wrapFactArg flag
  <|> wrapFactArg commodity
  <|> wrapFactArg commodityLevel
  <|> wrapFactArg commodityAny
  <|> wrapFactArg postingMemo
  <|> wrapFactArg transactionMemo
  <|> (do { o <- debit; return (\_ _ _ _ _ -> return o) })
  <|> (do { o <- credit; return (\_ _ _ _ _ -> return o) })
  <|> (do { f <- qtyOption; return (\ _ _ rg _ _ -> f rg)})
  <|> wrapNoArg globalTransaction
  <|> wrapNoArg globalPosting
  <|> wrapNoArg filePosting
  <|> wrapNoArg fileTransaction


wrapNoArg ::
  Parser (Ex.Exceptional Error Operand)
  -> Parser (L.DateTime
             -> DefaultTimeZone
             -> RadGroup
             -> CaseSensitive
             -> MatcherFactory
             -> Ex.Exceptional Error Operand)
wrapNoArg p = do
  o <- p
  return (\_ _ _ _ _ -> o)

wrapFactArg ::
  Parser (CaseSensitive -> MatcherFactory -> Exceptional Error Operand)
  -> Parser (L.DateTime
             -> DefaultTimeZone
             -> RadGroup
             -> CaseSensitive
             -> MatcherFactory
             -> Ex.Exceptional Error Operand)
wrapFactArg p = do
  f <- p
  return (\_ _ _ cs fact -> f cs fact)

------------------------------------------------------------
-- Post filters
------------------------------------------------------------

optHead :: Parser (Exceptional Error PostFilterFn)
optHead =
  let os = C.OptSpec ["head"] [] (C.OneArg f)
      f a = do
        i <- fmap ItemIndex (parseInt a)
        return (\_ idx -> idx < i)
  in C.parseOption [os]

optTail :: Parser (Exceptional Error PostFilterFn)
optTail =
  let os = C.OptSpec ["tail"] [] (C.OneArg f)
      f a = do
        i <- parseInt a
        let g (ListLength len) (ItemIndex idx) = idx >= len - i
        return g
  in C.parseOption [os]

parsePostFilter :: Parser (Exceptional Error PostFilterFn)
parsePostFilter = optHead <|> optTail

------------------------------------------------------------
-- Matcher control
------------------------------------------------------------

noArg :: a -> String -> Parser a
noArg a s = let os = C.OptSpec [s] "" (C.NoArg a)
            in C.parseOption [os]

parseInsensitive :: Parser CaseSensitive
parseInsensitive =
  let os = C.OptSpec ["case-insensitive"] ['i'] (C.NoArg Insensitive)
  in C.parseOption [os]

parseSensitive :: Parser CaseSensitive
parseSensitive =
  let os = C.OptSpec ["case-sensitive"] ['I'] (C.NoArg Sensitive)
  in C.parseOption [os]


within :: Parser MatcherFactory
within = noArg (\c t -> return (TM.within c t)) "within"

pcre :: Parser MatcherFactory
pcre = noArg TM.pcre "pcre"

posix :: Parser MatcherFactory
posix = noArg TM.tdfa "posix"

exact :: Parser MatcherFactory
exact = noArg (\c t -> return (TM.exact c t)) "exact"

parseMatcherSelect :: Parser MatcherFactory
parseMatcherSelect = within <|> pcre <|> posix <|> exact

parseCaseSelect :: Parser CaseSensitive
parseCaseSelect = parseInsensitive <|> parseSensitive

------------------------------------------------------------
-- Operators
------------------------------------------------------------

-- | Open parentheses
open :: Parser (X.Token a)
open = noArg X.TokOpenParen "open"

-- | Close parentheses
close :: Parser (X.Token a)
close = noArg X.TokCloseParen "close"

-- | and operator
parseAnd :: Parser (X.Token (a -> Bool))
parseAnd = noArg X.tokAnd "and"

-- | or operator
parseOr :: Parser (X.Token (a -> Bool))
parseOr = noArg X.tokOr "or"

-- | not operator
parseNot :: Parser (X.Token (a -> Bool))
parseNot = noArg X.tokNot "not" where

parseOperator :: Parser (X.Token (a -> Bool))
parseOperator =
  open <|> close <|> parseAnd <|> parseOr <|> parseNot

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


parseSort :: Parser (Exceptional Error Orderer)
parseSort =
  let os = C.OptSpec ["sort"] ['s'] (C.OneArg f)
      f a =
        let matches = filter (\p -> a `isPrefixOf` (fst p)) ords
        in case matches of
          [] -> Ex.throw $ BadSortKeyError (pack a)
          x:[] -> return $ snd x
          _ -> Ex.throw $ BadSortKeyError (pack a)
  in C.parseOption [os]
