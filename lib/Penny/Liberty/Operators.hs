module Penny.Liberty.Operators where

import Control.Applicative ((<|>))
import Data.List (intersperse, groupBy)
import qualified Penny.Liberty.Queue as Q
import Data.Text (pack)
import System.Console.MultiArg.Combinator (
  OptSpec(OptSpec), ArgSpec(NoArg),
  parseOption)
import System.Console.MultiArg.Option (makeLongOpt)
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Liberty.Expressions as X
import qualified Control.Monad.Exception.Synchronous as Ex
import Penny.Liberty.Error (Error)

noArg :: a -> String -> Parser a
noArg a s = let os = OptSpec [s] "" (NoArg a)
            in parseOption [os]

-- | Open parentheses
open :: Parser (X.Token a)
open = noArg X.TokOpenParen "open"

-- | Close parentheses
close :: Parser (X.Token a)
close = noArg X.TokCloseParen "close"

-- | and operator
parseAnd :: Parser (X.Token (a -> Bool))
parseAnd = noArg tokAnd "and"

tokAnd :: X.Token (a -> Bool)
tokAnd = X.TokBinary (X.Precedence 3) X.ALeft f where
  f x y = \a -> x a && y a

-- | or operator
parseOr :: Parser (X.Token (a -> Bool))
parseOr = noArg tokOr "or" where
  f x y = \a -> x a || y a
  tokOr = X.TokBinary (X.Precedence 2) X.ALeft f

-- | not operator
parseNot :: Parser (X.Token (a -> Bool))
parseNot = noArg tokNot "not" where
  tokNot = X.TokUnaryPrefix (X.Precedence 4) (not .)


-- | Takes the list of tokens and gets the predicate to use.
getPredicate :: 
  [X.Token (a -> Bool)]
  -> Maybe (a -> Bool)
getPredicate ls =
  if null ls then Just (const True) else X.evaluate q where
    q = foldl (flip Q.enqueue) Q.empty (insertAddTokens ls)

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

parser :: Parser (X.Token (a -> Bool))
parser =
  open
  <|> close
  <|> parseAnd
  <|> parseOr
  <|> parseNot
