-- | The Zinc filter.
--
-- The Zinc command line can be broken into three parts: the filter
-- specification, the report specification, and the files
-- specification. This module handles the filter specification. Its
-- job is to build a single predicate that will take each PostingInfo
-- and return True if the PostingInfo shall be included in the list of
-- postings or False if it is not.
--
-- This filter is used by other parts of Penny, too--the Postings
-- report also uses this module to filter out which postings should
-- appear in the report. So, this filter has only options that apply
-- to any PostingBox. Other options are inside other modules. For
-- instance the main filter specification also allows options like
-- --fwd-seq-unsorted; these are handled elsewhere.
--
-- All options that the filter takes do one of two things: either they
-- affect the matcher that is used to determine whether textual fields
-- are a match, or they add tokens that are later parsed by an
-- expression parser.
module Penny.Zinc.Parser.Filter where

import Control.Applicative ((<|>))
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.List (intersperse, groupBy)
import Data.Monoid (mempty)
import Data.Queue (enqueue, empty)
import Data.Text (Text, pack, unpack)
import System.Console.MultiArg.Combinator
  (mixedNoArg, mixedOneArg, longOneArg, longNoArg, longTwoArg,
   mixedTwoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
import System.Console.MultiArg.Prim (ParserE, throw)
import qualified Text.Matchers.Text as M
import Text.Parsec (parse)

import Penny.Copper.DateTime (DefaultTimeZone, dateTime)
import Penny.Copper.Qty (Radix, Separator, qty)

import qualified Penny.Lincoln.Predicates as P
import Penny.Lincoln.Bits (DateTime, Qty)
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Zinc.Expressions as X

import Penny.Zinc.Parser.Error (Error)
import qualified Penny.Zinc.Parser.Error as E


-- * The state

-- | All options share this common state. Additional tokens are
-- appended to the end of the tokens list (perhaps they should be
-- prepended and then reversed later, or perhaps a Queue should be
-- used?)
data State =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingBox -> Bool)] }

blankState :: State
blankState = State { sensitive = M.Insensitive
                   , matcher = return . M.within M.Insensitive
                   , tokens = mempty }

-- | Appends a token to the end of the token list.
addToken :: X.Token (PostingBox -> Bool) -> State -> State
addToken t s = s { tokens = tokens s ++ [t] }

-- | Adds an additional operand to the state. Operands examine a
-- PostingBox and return a boolean.
addOperand :: (PostingBox -> Bool) -> State -> State
addOperand = addToken . X.TokOperand

-- * MultiArg option factories

-- | Creates options that match against fields that are
-- colon-separated. The operand added will return True if the
-- posting's colon-separated option matches the pattern given, or
-- False if it does not.
sepOption ::
  String
  -- ^ Long option name
  
  -> Maybe Char
  -- ^ Short option name, if there is one

  -> (Text -> (Text -> Bool) -> PostingBox -> Bool)
  -- ^ When applied to a text that separates the different segments of
  -- the field, a matcher, and a posting, this function returns True
  -- if the posting matches the matcher or False if it does not.
  
  -> State
  -- ^ The initial state

  -> ParserE Error (State)
  -- ^ The final state, with the appropriate operand added.
sepOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    Nothing -> []
    (Just c) -> [makeShortOpt c]
  m <- throwIf $ getMatcher p s
  return $ addOperand (f sep m) s

-- | Creates options that take two arguments, with the first argument
-- being the level to match, and the second being the pattern that
-- should match against that level.  Adds an operand that returns True
-- if the pattern matches.
levelOption ::
  String
  -- ^ Long option name
  
  -> (Int -> (Text -> Bool) -> PostingBox -> Bool)
  -- ^ Applied to an integer, a matcher, and a PostingBox, this
  -- function returns True if a particular field in the posting
  -- matches the matcher given at the given level, or False otherwise.

  -> State
  -- ^ Initial state
  
  -> ParserE Error (State)
  -- ^ Final state, with the appropriate operand added.
levelOption str f s = do
  let lo = makeLongOpt . pack $ str
  (_, ns, p) <- longTwoArg lo
  n <- throwIf $ parseInt ns
  m <- throwIf $ getMatcher p s
  return $ addOperand (f n m) s

-- | Creates options that add an operand that matches the posting if a
-- particluar field matches the pattern given.
patternOption ::
  String
  -- ^ Long option
  
  -> Maybe Char
  -- ^ Short option, if included

  -> ((Text -> Bool) -> PostingBox -> Bool)
  -- ^ When applied to a matcher and a PostingBox, this function
  -- returns True if the posting matches, or False if it does not.
  
  -> State
  -- ^ The initial state
  
  -> ParserE Error (State)
  -- ^ The final state, with the option added
patternOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    (Just c) -> [makeShortOpt c]
    Nothing -> []
  m <- throwIf $ getMatcher p s
  return $ addOperand (f m) s

-- | Creates options that change the common state in some way.
changeState ::
  String
  -- ^ Long option name

  -> Maybe Char
  -- ^ Short option name, if wanted

  -> (State -> State)
  -- ^ State transformer
  
  -> State
  -- ^ Initial state

  -> ParserE Error (State)
  -- ^ Final state
changeState str mc f s = do
  let lo = makeLongOpt . pack $ str
      so = case mc of
        Nothing -> []
        Just c -> [makeShortOpt c]
  _ <- mixedNoArg lo [] so
  return $ f s

-- * Miscellaneous combinators

-- | Throws if the function returned an Exception; otherwise, returns
-- the value of the Success. Useful for applying functions from within
-- the ParserE monad that might fail but that do not need access to
-- the ParserE monad.
throwIf :: Exceptional Error g -> ParserE Error g
throwIf ex = case ex of
  Exception e -> throw e
  Success g -> return g

-- * Comparison options

-- | Parses comparers given on command line to internal representation
parseComparer :: Text -> Exceptional E.Error P.Comparer
parseComparer t
  | t == pack "<" = Success P.LessThan
  | t == pack "<=" = Success P.LessThanEQ
  | t == pack "==" = Success P.Equals
  | t == pack ">" = Success P.GreaterThan
  | t == pack ">=" = Success P.GreaterThanEQ
  | t == pack "/=" = Success P.NotEquals
  | otherwise = Exception $ E.BadComparator t

-- * Dates

date :: DefaultTimeZone -> State -> ParserE Error State
date dtz s = do
  let lo = makeLongOpt . pack $ "date"
      so = makeShortOpt 'd'
  (_, c, d) <- mixedTwoArg lo [] [so]
  cmp <- throwIf $ parseComparer c
  dt <- throwIf $ parseDate dtz d
  return $ addOperand (P.date cmp dt) s

current :: DateTime -> State -> ParserE Error (State)
current dt s = do
  let lo = makeLongOpt . pack $ "current"
      cmp = P.LessThanEQ
  _ <- longNoArg lo
  return $ addOperand (P.date cmp dt) s

parseDate :: DefaultTimeZone -> Text -> Exceptional Error DateTime
parseDate dtz t = case parse (dateTime dtz) "" t of
  Left _ -> Exception E.DateParseError
  Right d -> return d

-- * Pattern matching

-- | Given a Text from the command line which represents a pattern,
-- and a State, return a Matcher. This will fail if the pattern is bad
-- (e.g. it is a bad regular expression).
getMatcher :: Text -> State -> Exceptional Error (Text -> Bool)
getMatcher t s = case matcher s t of
  Exception e -> Exception $ E.BadPatternError e
  Success m -> return m

sep :: Text
sep = pack ":"

-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: State -> ParserE Error (State)
account = sepOption "account" (Just 'A') P.account

-- | Parses exactly one integer; fails if it cannot read exactly one.
parseInt :: Text -> Exceptional Error Int
parseInt t = let ps = reads . unpack $ t in
  case ps of
    [] -> Exception $ E.BadNumberError t
    ((i, s):[]) -> if length s /= 0
                   then Exception $ E.BadNumberError t
                   else return i
    _ -> Exception $ E.BadNumberError t

-- | The account-level option; matches if the account at the given
-- level matches.
accountLevel :: State -> ParserE Error (State)
accountLevel = levelOption "account-level" P.accountLevel

-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: State -> ParserE Error (State)
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: State -> ParserE Error (State)
payee = patternOption "payee" (Just 'p') P.payee

tag :: State -> ParserE Error (State)
tag = patternOption "tag" (Just 't') P.tag

number :: State -> ParserE Error (State)
number = patternOption "number" Nothing P.number

flag :: State -> ParserE Error (State)
flag = patternOption "flag" Nothing P.flag

commodity :: State -> ParserE Error (State)
commodity = sepOption "commodity" Nothing P.commodity

commodityLevel :: State -> ParserE Error (State)
commodityLevel = levelOption "commodity-level" P.commodityLevel

commodityAny :: State -> ParserE Error (State)
commodityAny = patternOption "commodity" Nothing P.commodityAny

postingMemo :: State -> ParserE Error (State)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: State -> ParserE Error (State)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

-- * Non-pattern matching

debit :: State -> ParserE Error (State)
debit = return . addOperand P.debit

credit :: State -> ParserE Error (State)
credit = return . addOperand P.credit

qtyOption ::
  Radix
  -> Separator
  -> State
  -> ParserE Error State
qtyOption rad sp s = do
  let lo = makeLongOpt . pack $ "qty"
  (_, cs, qs) <- longTwoArg lo
  q <- case parse (qty rad sp) "" qs of
    Left _ -> throw $ E.BadQtyError qs
    Right qtParsed -> return qtParsed
  c <- throwIf $ parseComparer cs
  return $ addOperand (P.qty c q) s

-- * Matcher manipulation

caseInsensitive :: State -> ParserE Error (State)
caseInsensitive = changeState "case-insensitive" (Just 'i') f where
  f st = st { sensitive = M.Insensitive }

caseSensitive :: State -> ParserE Error (State)
caseSensitive = changeState "case-sensitive" (Just 'I') f where
  f st = st { sensitive = M.Sensitive }

within :: State -> ParserE Error (State)
within = changeState "within" Nothing f where
  f st = st { matcher = \t -> return (M.within (sensitive st) t) }

pcre :: State -> ParserE Error (State)
pcre = changeState "pcre" Nothing f where
  f st = st { matcher = M.pcre (sensitive st) }

posix :: State -> ParserE Error (State)
posix = changeState "posix" Nothing f where
  f st = st { matcher = M.tdfa (sensitive st) }

exact :: State -> ParserE Error (State)
exact = changeState "exact" Nothing f where
  f st = st { matcher = \t -> return (M.exact (sensitive st) t) }

-- * Operators

-- | Open parentheses
open :: State -> ParserE Error (State)
open s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokOpenParen s)

-- | Close parentheses
close :: State -> ParserE Error (State)
close s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokCloseParen s)

-- | and operator
parseAnd :: State -> ParserE Error (State)
parseAnd s = do
  let lo = makeLongOpt . pack $ "and"
  _ <- longNoArg lo
  return (addToken tokAnd s)

tokAnd :: X.Token (a -> Bool)
tokAnd = X.TokBinary (X.Precedence 3) X.ALeft f where
  f x y = \a -> x a && y a

-- | or operator
parseOr :: State -> ParserE Error (State)
parseOr s = do
  let lo = makeLongOpt . pack $ "or"
  _ <- longNoArg lo
  let f x y = \a -> x a || y a
  return (addToken (X.TokBinary (X.Precedence 2) X.ALeft f) s)

-- | not operator
parseNot :: State -> ParserE Error (State)
parseNot s = do
  let lo = makeLongOpt . pack $ "not"
  _ <- longNoArg lo
  let f = (not .)
  return (addToken (X.TokUnaryPrefix (X.Precedence 4) f) s)

-- | Operands that are not separated by operators are assumed to be
-- joined with an and operator; this function adds the and operators.
insertAddTokens :: [X.Token (a -> Bool)]
                   -> [X.Token (a -> Bool)]
insertAddTokens ts = concatMap inserter grouped where
  inserter = intersperse tokAnd
  grouped = groupBy f ts
  f x y = case (x, y) of
    (X.TokOperand _, X.TokOperand _) -> True
    _ -> False

-- | Takes the list of tokens and gets the predicate to use.
getPredicate :: State -> Maybe (PostingBox -> Bool)
getPredicate s = X.evaluate q where
  q = foldl (flip enqueue) empty (insertAddTokens . tokens $ s)

-- * The combined token parser

-- | Combines all the parsers in this module to parse a single
-- token. Only parses one token. Fails if the next word on the command
-- line is not a token.
parseToken :: DefaultTimeZone
              -> DateTime
              -> Radix
              -> Separator
              -> State
              -> ParserE Error (State)
parseToken dtz dt rad sp st =
  date dtz st
  <|> current dt st
  
  <|> account st
  <|> accountLevel st
  <|> accountAny st
  <|> payee st
  <|> tag st
  <|> number st
  <|> flag st
  <|> commodity st
  <|> commodityLevel st
  <|> commodityAny st
  <|> postingMemo st
  <|> transactionMemo st
  <|> debit st
  <|> credit st
  
  <|> qtyOption rad sp st
  
  <|> caseInsensitive st
  <|> caseSensitive st
  <|> within st
  <|> pcre st
  <|> posix st
  <|> exact st
  
  <|> open st
  <|> close st
  <|> parseAnd st
  <|> parseOr st
  <|> parseNot st

