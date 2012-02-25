module Penny.Liberty.PostFilters where

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

import qualified Penny.Liberty.Expressions as X
import qualified Penny.Liberty.Matchers as PM
import qualified Penny.Liberty.Operands as O
import qualified Penny.Liberty.Operators as Oo
import qualified Penny.Liberty.Sorter as S
import qualified Penny.Liberty.Types as T
import Penny.Liberty.Error (Error)

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
