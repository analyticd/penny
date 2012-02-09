module Penny.Zinc.Parser.Filter where

import Control.Applicative ((<|>))
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.List (intersperse, groupBy)
import Data.Monoid (mempty, mappend)
import Data.Monoid.Extra (Any(Any), All(All),
                          appAny, appAll)
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


data State t p =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingBox t p -> Bool)] }

blankState :: State t p
blankState = State { sensitive = M.Insensitive
                   , matcher = return . M.within M.Insensitive
                   , tokens = mempty }

addToken :: X.Token (PostingBox t p -> Bool) -> State t p -> State t p
addToken t s = s { tokens = tokens s ++ [t] }

addOperand :: (PostingBox t p -> Bool) -> State t p -> State t p
addOperand = addToken . X.TokOperand

before :: DefaultTimeZone -> State t p -> ParserE Error (State t p)
before dtz s = do
  let lo = makeLongOpt . pack $ "before"
  (_, t) <- mixedOneArg lo [] []
  dt <- parseDate dtz t
  return $ addOperand (P.before dt) s

after :: DefaultTimeZone -> State t p -> ParserE Error (State t p)
after dtz s = do
  let lo = makeLongOpt . pack $ "after"
  (_, t) <- longOneArg lo
  d <- parseDate dtz t
  return $ addOperand (P.after d) s

onOrBefore :: DefaultTimeZone -> State t p -> ParserE Error (State t p)
onOrBefore dtz s = do
  let lo = makeLongOpt . pack $ "on-or-before"
      so = makeShortOpt 'b'
  (_, t) <- mixedOneArg lo [] [so]
  d <- parseDate dtz t
  return $ addOperand (P.onOrBefore d) s

onOrAfter :: DefaultTimeZone -> State t p -> ParserE Error (State t p)
onOrAfter dtz s = do
  let lo = makeLongOpt . pack $ "on-or-after"
      so = makeShortOpt 'a'
  (_, t) <- mixedOneArg lo [] [so]
  d <- parseDate dtz t
  return $ addOperand (P.onOrAfter d) s
  
dayEquals :: DefaultTimeZone -> State t p -> ParserE Error (State t p)
dayEquals dtz s = do
  let lo = makeLongOpt . pack $ "day-equals"
  (_, t) <- longOneArg lo
  d <- parseDate dtz t
  return $ addOperand (P.dateIs d) s

current :: DateTime -> State t p -> ParserE Error (State t p)
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

getMatcher :: Text -> State t p -> ParserE Error (Text -> Bool)
getMatcher t s = case matcher s t of
  Exception e -> throw $ E.BadPatternError e
  Success m -> return m

sep :: Text
sep = pack ":"

sepOption ::
  String
  -> Maybe Char
  -> (Text -> (Text -> Bool) -> PostingBox t p -> Bool)
  -> State t p
  -> ParserE Error (State t p)
sepOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    Nothing -> []
    (Just c) -> [makeShortOpt c]
  m <- getMatcher p s
  return $ addOperand (f sep m) s

account :: State t p -> ParserE Error (State t p)
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
  -> (Int -> (Text -> Bool) -> PostingBox t p -> Bool)
  -> State t p
  -> ParserE Error (State t p)
levelOption str f s = do
  let lo = makeLongOpt . pack $ str
  (_, ns, p) <- longTwoArg lo
  n <- parseInt ns
  m <- getMatcher p s
  return $ addOperand (f n m) s

accountLevel :: State t p -> ParserE Error (State t p)
accountLevel = levelOption "account-level" P.accountLevel

accountAny :: State t p -> ParserE Error (State t p)
accountAny = patternOption "account-any" Nothing P.accountAny

payee :: State t p -> ParserE Error (State t p)
payee = patternOption "payee" (Just 'p') P.payee

patternOption ::
  String -- ^ Long option
  -> Maybe Char -- ^ Short option
  -> ((Text -> Bool) -> PostingBox t p -> Bool) -- ^ Predicate maker
  -> State t p
  -> ParserE Error (State t p)
patternOption str mc f s = do
  let lo = makeLongOpt . pack $ str
  (_, p) <- mixedOneArg lo [] $ case mc of
    (Just c) -> [makeShortOpt c]
    Nothing -> []
  m <- getMatcher p s
  return $ addOperand (f m) s

tag :: State t p -> ParserE Error (State t p)
tag = patternOption "tag" (Just 't') P.tag

number :: State t p -> ParserE Error (State t p)
number = patternOption "number" Nothing P.number

flag :: State t p -> ParserE Error (State t p)
flag = patternOption "flag" Nothing P.flag

commodity :: State t p -> ParserE Error (State t p)
commodity = sepOption "commodity" Nothing P.commodity

commodityLevel :: State t p -> ParserE Error (State t p)
commodityLevel = levelOption "commodity-level" P.commodityLevel

commodityAny :: State t p -> ParserE Error (State t p)
commodityAny = patternOption "commodity" Nothing P.commodityAny


postingMemo :: State t p -> ParserE Error (State t p)
postingMemo = patternOption "posting-memo" Nothing P.postingMemo

transactionMemo :: State t p -> ParserE Error (State t p)
transactionMemo = patternOption "transaction-memo"
                  Nothing P.transactionMemo

noFlag :: State t p -> ParserE Error (State t p)
noFlag = return . addOperand P.noFlag

debit :: State t p -> ParserE Error (State t p)
debit = return . addOperand P.debit

credit :: State t p -> ParserE Error (State t p)
credit = return . addOperand P.credit

qtyOption ::
  String
  -> (Qty -> PostingBox t p -> Bool)
  -> Radix
  -> Separator
  -> State t p
  -> ParserE Error (State t p)
qtyOption str f rad sp s = do
  let lo = makeLongOpt . pack $ str
  (_, qs) <- longOneArg lo
  case parse (qty rad sp) "" qs of
    Left _ -> throw $ E.BadQtyError qs
    Right qt -> return $ addOperand (f qt) s

atLeast ::
  Radix
  -> Separator
  -> State t p
  -> ParserE Error (State t p)
atLeast = qtyOption "at-least" P.greaterThanOrEqualTo

lessThan ::
  Radix
  -> Separator
  -> State t p
  -> ParserE Error (State t p)
lessThan = qtyOption "less-than" P.lessThan

equals ::
  Radix
  -> Separator
  -> State t p
  -> ParserE Error (State t p)
equals = qtyOption "equals" P.equals

changeState ::
  String
  -> Maybe Char
  -> (State t p -> State t p)
  -> State t p
  -> ParserE Error (State t p)
changeState str mc f s = do
  let lo = makeLongOpt . pack $ str
      so = case mc of
        Nothing -> []
        Just c -> [makeShortOpt c]
  _ <- mixedNoArg lo [] so
  return $ f s


caseInsensitive :: State t p -> ParserE Error (State t p)
caseInsensitive = changeState "case-insensitive" (Just 'i') f where
  f st = st { sensitive = M.Insensitive }

caseSensitive :: State t p -> ParserE Error (State t p)
caseSensitive = changeState "case-sensitive" (Just 'I') f where
  f st = st { sensitive = M.Sensitive }

within :: State t p -> ParserE Error (State t p)
within = changeState "within" Nothing f where
  f st = st { matcher = \t -> return (M.within (sensitive st) t) }

pcre :: State t p -> ParserE Error (State t p)
pcre = changeState "pcre" Nothing f where
  f st = st { matcher = M.pcre (sensitive st) }

posix :: State t p -> ParserE Error (State t p)
posix = changeState "posix" Nothing f where
  f st = st { matcher = M.tdfa (sensitive st) }

exact :: State t p -> ParserE Error (State t p)
exact = changeState "exact" Nothing f where
  f st = st { matcher = \t -> return (M.exact (sensitive st) t) }

open :: State t p -> ParserE Error (State t p)
open s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokOpenParen s)

close :: State t p -> ParserE Error (State t p)
close s = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return (addToken X.TokCloseParen s)

parseAnd :: State t p -> ParserE Error (State t p)
parseAnd s = do
  let lo = makeLongOpt . pack $ "and"
  _ <- longNoArg lo
  return (addToken tokAnd s)

tokAnd :: X.Token (a -> Bool)
tokAnd = X.TokBinary (X.Precedence 3) X.ALeft f where
  f x y = appAll (All x `mappend` All y)

parseOr :: State t p -> ParserE Error (State t p)
parseOr s = do
  let lo = makeLongOpt . pack $ "or"
  _ <- longNoArg lo
  let f x y = appAny (Any x `mappend` Any y)
  return (addToken (X.TokBinary (X.Precedence 2) X.ALeft f) s)

parseNot :: State t p -> ParserE Error (State t p)
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

getPredicate :: State t p -> Maybe (PostingBox t p -> Bool)
getPredicate s = X.evaluate q where
  q = foldl (flip X.enqueue) X.empty (insertAddTokens . tokens $ s)

parseToken :: DefaultTimeZone
              -> DateTime
              -> Radix
              -> Separator
              -> State t p
              -> ParserE Error (State t p)
parseToken dtz dt rad sep st =
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
  
  <|> atLeast rad sep st
  <|> lessThan rad sep st
  <|> equals rad sep st
  
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

