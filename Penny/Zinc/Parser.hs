module Penny.Zinc.Parser where

import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.Monoid (mempty, mappend)
import Data.Monoid.Extra (Orderer(appOrderer))
import Data.Text (Text, pack, unpack)
import System.Console.MultiArg.Combinator
  (mixedOneArg, longOneArg, longNoArg, longTwoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Prim (ParserE, throw)
import qualified Text.Matchers.Text as M
import Text.Parsec (parse)

import Penny.Copper.DateTime (DefaultTimeZone, dateTime)

import Penny.Copper.Meta ( TransactionMeta, PostingMeta )
import qualified Penny.Lincoln.Predicates as P
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Zinc.Expressions as X

data Error = MultiArgError E.Expecting E.Saw
             | MakeMatcherFactoryError Text
             | DateParseError
             | BadPatternError Text
             | BadNumberError Text
             deriving Show

instance E.Error Error where
  parseErr = MultiArgError

data State t p =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingBox t p -> Bool)]
        , sorter :: Orderer (PostingBox t p -> Ordering) }

blankState :: State t p
blankState = State { sensitive = M.Insensitive
                   , matcher = return . M.within M.Insensitive
                   , tokens = mempty
                   , sorter = mempty }

addOperand :: (PostingBox t p -> Bool) -> State t p -> State t p
addOperand f s = s { tokens = tokens s ++ [X.TokOperand f] }

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
  Left _ -> throw DateParseError
  Right d -> return d

--
-- Pattern matching
--

getMatcher :: Text -> State t p -> ParserE Error (Text -> Bool)
getMatcher t s = case matcher s t of
  Exception e -> throw $ BadPatternError e
  Success m -> return m

sep :: Text
sep = pack ":"

account :: State t p -> ParserE Error (State t p)
account s = do
  let lo = makeLongOpt . pack $ "account"
      so = makeShortOpt 'A'
  (_, p) <- mixedOneArg lo [] [so]
  m <- getMatcher p s
  return $ addOperand (P.account sep m) s

parseInt :: Text -> ParserE Error Int
parseInt t = let ps = reads . unpack $ t in
  case ps of
    [] -> throw $ BadNumberError t
    ((i, s):[]) -> if length s /= 0
                   then throw $ BadNumberError t
                   else return i
    _ -> throw $ BadNumberError t

accountLevel :: State t p -> ParserE Error (State t p)
accountLevel s = do
  let lo = makeLongOpt . pack $ "account-level"
  (_, ns, p) <- longTwoArg lo
  n <- parseInt ns
  m <- getMatcher p s
  return $ addOperand (P.accountLevel n m) s

accountAny :: State t p -> ParserE Error (State t p)
accountAny s = do
  let lo = makeLongOpt . pack $ "account-any"
  (_, p) <- longOneArg lo
  m <- getMatcher p s
  return $ addOperand (P.accountAny m) s

payee :: State t p -> ParserE Error (State t p)
payee s = do
  let lo = makeLongOpt . pack $ "payee"
      so = makeShortOpt 'p'
  (_, p) <- mixedOneArg lo [] [so]
  m <- getMatcher p s
  return $ addOperand (P.payee m) s
