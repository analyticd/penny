module Penny.Zinc.Parser.PostFilters where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success))
import Data.List (sortBy)
import Data.Monoid (mempty, mappend)
import Data.Text (Text, pack)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed)
import System.Console.MultiArg.Combinator
  (mixedNoArg, mixedOneArg, longOneArg, longNoArg, longTwoArg,
   mixedTwoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)

import qualified Penny.Zinc.Expressions as X
import qualified Penny.Zinc.Parser.Matchers as PM
import qualified Penny.Zinc.Parser.Operands as O
import qualified Penny.Zinc.Parser.Operators as Oo
import qualified Penny.Zinc.Parser.Sorter as S
import qualified Penny.Zinc.Parser.Types as T
import Penny.Zinc.Parser.Error (Error)

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Predicates as P

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
