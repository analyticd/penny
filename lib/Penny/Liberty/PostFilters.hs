module Penny.Liberty.PostFilters where

import Control.Applicative ((<|>))
import Data.Text (pack)
import System.Console.MultiArg.Prim (ParserE)
import System.Console.MultiArg.Combinator (longOneArg)
import System.Console.MultiArg.Option (makeLongOpt)

import qualified Penny.Liberty.Operands as O
import Penny.Liberty.Error (Error)

optHead :: ParserE Error ([a] -> [a])
optHead = do
  let lo = makeLongOpt . pack $ "head"
  (_, iStr) <- longOneArg lo
  i <- O.throwIf (O.parseInt iStr)
  let f ls = take i ls
  return f

optTail :: ParserE Error ([a] -> [a])
optTail = do
  let lo = makeLongOpt . pack $ "tail"
  (_, iStr) <- longOneArg lo
  i <- O.throwIf (O.parseInt iStr)
  let f ls = drop toDrop ls where
        toDrop = length ls - i
  return f

parser :: ParserE Error ([a] -> [a])
parser = optHead <|> optTail
