module Penny.Zinc.Parser.Operators where

import Control.Applicative ((<|>))
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.List (intersperse, groupBy)
import Data.Text (Text, pack, unpack)
import System.Console.MultiArg.Combinator
  (mixedNoArg, mixedOneArg, longOneArg, longNoArg, longTwoArg,
   mixedTwoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
import System.Console.MultiArg.Prim (ParserE, throw)

import Penny.Copper.DateTime (DefaultTimeZone, dateTime)
import qualified Penny.Zinc.Expressions as X
import Penny.Zinc.Parser.Error (Error)
import qualified Penny.Zinc.Parser.Error as E

-- | Open parentheses
open :: ParserE Error (X.Token a)
open = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return X.TokOpenParen

-- | Close parentheses
close :: ParserE Error (X.Token a)
close = let lo = makeLongOpt . pack $ "open" in
  longNoArg lo >> return X.TokCloseParen

-- | and operator
parseAnd :: ParserE Error (X.Token (a -> Bool))
parseAnd = do
  let lo = makeLongOpt . pack $ "and"
  _ <- longNoArg lo
  return tokAnd

tokAnd :: X.Token (a -> Bool)
tokAnd = X.TokBinary (X.Precedence 3) X.ALeft f where
  f x y = \a -> x a && y a

-- | or operator
parseOr :: ParserE Error (X.Token (a -> Bool))
parseOr = do
  let lo = makeLongOpt . pack $ "or"
  _ <- longNoArg lo
  let f x y = \a -> x a || y a
  return (X.TokBinary (X.Precedence 2) X.ALeft f)

-- | not operator
parseNot :: ParserE Error (X.Token (a -> Bool))
parseNot = do
  let lo = makeLongOpt . pack $ "not"
  _ <- longNoArg lo
  let f = (not .)
  return (X.TokUnaryPrefix (X.Precedence 4) f)

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

parser :: ParserE Error (X.Token (a -> Bool))
parser =
  open
  <|> close
  <|> parseAnd
  <|> parseOr
  <|> parseNot
