module Penny.Zinc.Parser.Filter where

import Control.Applicative ((<|>))
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.List (intersperse, groupBy)
import Data.Monoid (mempty)
import Data.Queue (enqueue, empty)
import Data.Text (Text, pack, unpack)
import System.Console.MultiArg.Combinator
  (mixedNoArg, mixedOneArg, longOneArg, longNoArg, longTwoArg)
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


data State =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingBox -> Bool)] }

blankState :: State
blankState = State { sensitive = M.Insensitive
                   , matcher = return . M.within M.Insensitive
                   , tokens = mempty }

addToken :: X.Token (PostingBox -> Bool) -> State -> State
addToken t s = s { tokens = tokens s ++ [t] }

addOperand :: (PostingBox -> Bool) -> State -> State
addOperand = addToken . X.TokOperand

before :: DefaultTimeZone -> State -> ParserE Error (State)
before dtz s = do
  let lo = makeLongOpt . pack $ "before"
  (_, t) <- mixedOneArg lo [] []
  dt <- parseDate dtz t
  return $ addOperand (P.before dt) s

after :: DefaultTimeZone -> State -> ParserE Error (State)
after dtz s = do
  let lo = makeLongOpt . pack $ "after"
  (_, t) <- longOneArg lo
  d <- parseDate dtz t
  return $ addOperand (P.after d) s

onOrBefore :: DefaultTimeZone -> State -> ParserE Error (State)
onOrBefore dtz s = do
  let lo = makeLongOpt . pack $ "on-or-before"
      so = makeShortOpt 'b'
  (_, t) <- mixedOneArg lo [] [so]
  d <- parseDate dtz t
  return $ addOperand (P.onOrBefore d) s

onOrAfter :: DefaultTimeZone -> State -> ParserE Error (State)
onOrAfter dtz s = do
  let lo = makeLongOpt . pack $ "on-or-after"
      so = makeShortOpt 'a'
  (_, t) <- mixedOneArg lo [] [so]
  d <- parseDate dtz t
  return $ addOperand (P.onOrAfter d) s
  
dayEquals :: DefaultTimeZone -> State -> ParserE Error (State)
dayEquals dtz s = do
  let lo = makeLongOpt . pack $ "day-equals"
  (_, t) <- longOneArg lo
  d <- parseDate dtz t
  return $ addOperand (P.dateIs d) s

current :: DateTime -> State -> ParserE Error (State)
current dt s = do
  let lo = makeLongOpt . pack $ "current"
  _ <- longNoArg lo
  return $ addOperand (P.onOrBefore dt) s

parseDate :: DefaultTimeZone -> Text -> ParserE Error DateTime
parseDate dtz t = case parse (dateTime dtz) "" t of
  Left _ -> throw E.DateParseError
  Right d -> return d

--
-- Pattern matching
--

getMatcher :: Text -> State -> ParserE Error (Text -> Bool)
getMatcher t s = case matcher s t of
  Exception e -> throw $ E.BadPatternError e
  Success m -> return m

sep :: Text
sep = pack ":"

sepOption ::
  String
  -> Maybe Char
  -> (Text -> (Text -> Bool) -> PostingBox -> Bool)
  -> State
  -> ParserE Error (State)
sepOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    Nothing -> []
    (Just c) -> [makeShortOpt c]
  m <- getMatcher p s
  return $ addOperand (f sep m) s

account :: State -> ParserE Error (State)
account = sepOption "account" (Just 'A') P.account

parseInt :: Text -> ParserE Error Int
parseInt t = let ps = reads . unpack $ t in
  case ps of
    [] -> throw $ E.BadNumberError t
    ((i, s):[]) -> if length s /= 0
                   then throw $ E.BadNumberError t
                   else return i
    _ -> throw $ E.BadNumberError t

levelOption ::
  String
  -> (Int -> (Text -> Bool) -> PostingBox -> Bool)
  -> State
  -> ParserE Error (State)
levelOption str f s = do
  let lo = makeLongOpt . pack $ str
  (_, ns, p) <- longTwoArg lo
  n <- parseInt ns
  m <- getMatcher p s
  return $ addOperand (f n m) s

accountLevel :: State -> ParserE Error (State)
accountLevel = levelOption "account-level" P.accountLevel

accountAny :: State -> ParserE Error (State)
accountAny = patternOption "account-any" Nothing P.accountAny

payee :: State -> ParserE Error (State)
payee = patternOption "payee" (Just 'p') P.payee

patternOption ::
  String -- ^ Long option
  -> Maybe Char -- ^ Short option
  -> ((Text -> Bool) -> PostingBox -> Bool) -- ^ Predicate maker
  -> State
  -> ParserE Error (State)
patternOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    (Just c) -> [makeShortOpt c]
    Nothing -> []
  m <- getMatcher p s
  return $ addOperand (f m) s

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

noFlag :: State -> ParserE Error (State)
noFlag = return . addOperand P.noFlag

debit :: State -> ParserE Error (State)
debit = return . addOperand P.debit

credit :: State -> ParserE Error (State)
credit = return . addOperand P.credit

qtyOption ::
  String
  -> (Qty -> PostingBox -> Bool)
  -> Radix
  -> Separator
  -> State
  -> ParserE Error (State)
qtyOption str f rad sp s = do
  let lo = makeLongOpt . pack $ str
  (_, qs) <- longOneArg lo
  case parse (qty rad sp) "" qs of
    Left _ -> throw $ E.BadQtyError qs
    Right qt -> return $ addOperand (f qt) s

atLeast ::
  Radix
  -> Separator
  -> State
  -> ParserE Error (State)
atLeast = qtyOption "at-least" P.greaterThanOrEqualTo

lessThan ::
  Radix
  -> Separator
  -> State
  -> ParserE Error (State)
lessThan = qtyOption "less-than" P.lessThan

equals ::
  Radix
  -> Separator
  -> State
  -> ParserE Error (State)
equals = qtyOption "equals" P.equals

changeState ::
  String
  -> Maybe Char
  -> (State -> State)
  -> State
  -> ParserE Error (State)
changeState str mc f s = do
  let lo = makeLongOpt . pack $ str
      so = case mc of
        Nothing -> []
        Just c -> [makeShortOpt c]
  _ <- mixedNoArg lo [] so
  return $ f s


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

open :: State -> ParserE Error (State)
open s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokOpenParen s)

close :: State -> ParserE Error (State)
close s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokCloseParen s)

parseAnd :: State -> ParserE Error (State)
parseAnd s = do
  let lo = makeLongOpt . pack $ "and"
  _ <- longNoArg lo
  return (addToken tokAnd s)

tokAnd :: X.Token (a -> Bool)
tokAnd = X.TokBinary (X.Precedence 3) X.ALeft f where
  f x y = \a -> x a && y a

parseOr :: State -> ParserE Error (State)
parseOr s = do
  let lo = makeLongOpt . pack $ "or"
  _ <- longNoArg lo
  let f x y = \a -> x a || y a
  return (addToken (X.TokBinary (X.Precedence 2) X.ALeft f) s)

parseNot :: State -> ParserE Error (State)
parseNot s = do
  let lo = makeLongOpt . pack $ "not"
  _ <- longNoArg lo
  let f = (not .)
  return (addToken (X.TokUnaryPrefix (X.Precedence 4) f) s)

insertAddTokens :: [X.Token (a -> Bool)]
                   -> [X.Token (a -> Bool)]
insertAddTokens ts = concatMap inserter grouped where
  inserter = intersperse tokAnd
  grouped = groupBy f ts
  f x y = case (x, y) of
    (X.TokOperand _, X.TokOperand _) -> True
    _ -> False

getPredicate :: State -> Maybe (PostingBox -> Bool)
getPredicate s = X.evaluate q where
  q = foldl (flip enqueue) empty (insertAddTokens . tokens $ s)

parseToken :: DefaultTimeZone
              -> DateTime
              -> Radix
              -> Separator
              -> State
              -> ParserE Error (State)
parseToken dtz dt rad sp st =
  before dtz st
  <|> after dtz st
  <|> onOrBefore dtz st
  <|> onOrAfter dtz st
  <|> dayEquals dtz st
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
  <|> noFlag st
  <|> debit st
  <|> credit st
  
  <|> atLeast rad sp st
  <|> lessThan rad sp st
  <|> equals rad sp st
  
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

