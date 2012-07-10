module Penny.Liberty.Operators (parser, getPredicate) where

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

parser :: Parser (X.Token (a -> Bool))
parser =
  open
  <|> close
  <|> parseAnd
  <|> parseOr
  <|> parseNot
