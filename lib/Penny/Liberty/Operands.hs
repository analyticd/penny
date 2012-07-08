-- | Zinc operands
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
module Penny.Liberty.Operands where

import Control.Applicative ((<|>), (<$))
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.Text (Text, pack, unpack)
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)
import Text.Parsec (parse)
  
import qualified Penny.Copper as Cop
import Penny.Copper.DateTime (DefaultTimeZone, dateTime)
import Penny.Copper.Qty (RadGroup, qty)

import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln as L
import qualified Penny.Liberty.Expressions as X

import Penny.Liberty.Error (Error)
import qualified Penny.Liberty.Error as E

type MatcherFactory = Text -> Ex.Exceptional Text (Text -> Bool)
type Operand =
  X.Operand (L.PostingChild Cop.TopLineMeta Cop.PostingMeta
             -> Bool)

type MatcherFunc =
  Parser (MatcherFactory -> Ex.Exceptional Error Operand)


-- | Given a Text from the command line which represents a pattern,
-- and a MatcherFactor, return a Matcher. Fails if the pattern is bad
-- (e.g. it is not a valid regular expression).
getMatcher ::
  Text
  -> MatcherFactory
  -> Ex.Exceptional Error (Text -> Bool)

getMatcher t f = case f t of
  Ex.Exception e -> Ex.Exception $ E.BadPatternError e
  Ex.Success m -> return m

-- | Parses comparers given on command line to a function.
parseComparer ::
  (Eq a, Ord a)
  => String
  -> Ex.Exceptional E.Error (a -> a -> Bool)
parseComparer t
  | t == "<" = Ex.Success (<)
  | t == "<=" = Ex.Success (<=)
  | t == "==" = Ex.Success (==)
  | t == ">" = Ex.Success (>)
  | t == ">=" = Ex.Success (>=)
  | t == "/=" = Ex.Success (/=)
  | t == "!=" = Ex.Success (/=)
  | otherwise = Ex.Exception $ E.BadComparator (pack t)

parseDate :: DefaultTimeZone -> Text -> Exceptional Error L.DateTime
parseDate dtz t = case parse (dateTime dtz) "" t of
  Left _ -> Exception E.DateParseError
  Right d -> return d

date :: DefaultTimeZone -> Parser (Ex.Exceptional Error Operand)
date dtz =
  let os = C.OptSpec ["date"] ['d'] (C.TwoArg f)
      f a1 a2 = do
        cmp <- parseComparer a1
        dt <- parseDate dtz (pack a2)
        return $ X.Operand (P.date cmp dt)
  in C.parseOption [os]

current :: Parser (DateTime -> Ex.Exceptional Error Operand)
current =
  let os = C.OptSpec ["current"] [] (C.NoArg f)
      
      f dt
  let lo = makeLongOpt . pack $ "current"
      cmp = P.LessThanEQ
  _ <- longNoArg lo
  return $ X.Operand (P.date cmp dt)



{-
type Operand = X.Operand (PostingBox -> Bool)
-- * The combined token parser

-- | Combines all the parsers in this module to parse a single
-- token. Only parses one token. Fails if the next word on the command
-- line is not a token.
parseToken :: DefaultTimeZone
              -> DateTime
              -> RadGroup
              -> MatcherFactory
              -> ParserE Error Operand
parseToken dtz dt rg f =
  date dtz
  <|> current dt
  
  <|> account f
  <|> accountLevel f
  <|> accountAny f
  <|> payee f
  <|> tag f
  <|> number f
  <|> flag f
  <|> commodity f
  <|> commodityLevel f
  <|> commodityAny f
  <|> postingMemo f
  <|> transactionMemo f
  <|> debit
  <|> credit
  
  <|> qtyOption rg

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
  
  -> MatcherFactory

  -> ParserE Error Operand
sepOption str mc f factory = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    Nothing -> []
    (Just c) -> [makeShortOpt c]
  m <- throwIf $ getMatcher p factory
  return $ X.Operand (f sep m)

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
  
  -> MatcherFactory
  
  -> ParserE Error (X.Operand (PostingBox -> Bool))
levelOption str f factory = do
  let lo = makeLongOpt . pack $ str
  (_, ns, p) <- longTwoArg lo
  n <- throwIf $ parseInt ns
  m <- throwIf $ getMatcher p factory
  return $ X.Operand (f n m)

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
  
  -> MatcherFactory
  -> ParserE Error Operand
patternOption str mc f factory = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    (Just c) -> [makeShortOpt c]
    Nothing -> []
  m <- throwIf $ getMatcher p factory
  return $ X.Operand (f m)

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

-- * Dates

-- * Pattern matching

sep :: Text
sep = pack ":"

-- | The account option; matches if the pattern given matches the
-- colon-separated account name.
account :: MatcherFactory -> ParserE Error Operand
account = sepOption "account" (Just 'a') P.account

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
accountLevel :: MatcherFactory -> ParserE Error Operand
accountLevel = levelOption "account-level" P.accountLevel

-- | The accountAny option; returns True if the matcher given matches
-- a single sub-account name at any level.
accountAny :: MatcherFactory -> ParserE Error Operand
accountAny = patternOption "account-any" Nothing P.accountAny

-- | The payee option; returns True if the matcher matches the payee
-- name.
payee :: MatcherFactory -> ParserE Error Operand
payee = patternOption "payee" (Just 'p') P.payee

tag :: MatcherFactory -> ParserE Error Operand
tag = patternOption "tag" (Just 't') P.tag

number :: MatcherFactory -> ParserE Error Operand
number = patternOption "number" Nothing P.number

flag :: MatcherFactory -> ParserE Error Operand
flag = patternOption "flag" Nothing P.flag

commodity :: MatcherFactory -> ParserE Error Operand
commodity = sepOption "commodity" Nothing P.commodity

commodityLevel :: MatcherFactory -> ParserE Error Operand
commodityLevel = levelOption "commodity-level" P.commodityLevel

commodityAny :: MatcherFactory -> ParserE Error Operand
commodityAny = patternOption "commodity" Nothing P.commodityAny

postingMemo :: MatcherFactory -> ParserE Error Operand
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: MatcherFactory -> ParserE Error Operand
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

-- * Non-pattern matching

debit :: ParserE Error Operand
debit = X.Operand P.debit
        <$ longNoArg (makeLongOpt . pack $ "debit")

credit :: ParserE Error Operand
credit = X.Operand P.credit
         <$ longNoArg (makeLongOpt . pack $ "credit")

qtyOption ::
  RadGroup
  -> ParserE Error Operand
qtyOption rg = do
  let lo = makeLongOpt . pack $ "qty"
  (_, cs, qs) <- longTwoArg lo
  q <- case parse (qty rg) "" qs of
    Left _ -> throw $ E.BadQtyError qs
    Right qtParsed -> return qtParsed
  c <- throwIf $ parseComparer cs
  return $ X.Operand (P.qty c q)


-}
