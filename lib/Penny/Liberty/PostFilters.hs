module Penny.Liberty.PostFilters where

import Control.Applicative ((<|>))
import Data.Text (pack)
import System.Console.MultiArg.Prim (ParserE)
import System.Console.MultiArg.Combinator (longOneArg)
import System.Console.MultiArg.Option (makeLongOpt)

import qualified Penny.Liberty.Operands as O
import qualified Penny.Liberty.Types as T
import Penny.Liberty.Error (Error)

optHead :: ParserE Error ([T.PostingInfo] -> [T.PostingInfo])
optHead = do
  let lo = makeLongOpt . pack $ "head"
  (_, iStr) <- longOneArg lo
  i <- O.throwIf (O.parseInt iStr)
  let f ls = take i ls
  return f

optTail :: ParserE Error ([T.PostingInfo] -> [T.PostingInfo])
optTail = do
  let lo = makeLongOpt . pack $ "tail"
  (_, iStr) <- longOneArg lo
  i <- O.throwIf (O.parseInt iStr)
  let f ls = drop toDrop ls where
        toDrop = length ls - i
  return f

parser :: ParserE Error ([T.PostingInfo] -> [T.PostingInfo])
parser = optHead <|> optTail
