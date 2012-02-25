module Penny.Liberty.Types where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success))
import Data.List (sortBy)
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed)

import qualified Penny.Liberty.Expressions as X
import qualified Penny.Liberty.Matchers as PM
import qualified Penny.Liberty.Operands as O
import qualified Penny.Liberty.Operators as Oo
import Penny.Liberty.Error (Error)

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)
import Penny.Lincoln.Predicates as P

data PostingInfo =
  PostingInfo { postingBox :: PostingBox
              , fwdSeqUnsorted :: FwdSeqUnsorted
              , backSeqUnsorted :: BackSeqUnsorted
              , fwdSeqSorted :: FwdSeqSorted
              , backSeqSorted :: BackSeqSorted }

newtype FwdSeqSorted = FwdSeqSorted { unFwdSeqSorted :: Integer }
                       deriving (Show, Eq, Ord)

newtype BackSeqSorted = BackSeqSorted { unBackSeqSorted :: Integer }
                      deriving (Show, Eq, Ord)

newtype FwdSeqUnsorted = FwdSeqUnsorted { unFwdSeqUnsorted :: Integer }
                       deriving (Show, Eq, Ord)

newtype BackSeqUnsorted = BackSeqUnsorted { unBackSeqUnsorted :: Integer }
                        deriving (Show, Eq, Ord)

testFwdSeqSorted ::
  P.Comparer -> FwdSeqSorted -> PostingInfo -> Bool
testFwdSeqSorted c f = P.comp c f fwdSeqSorted

testFwdSeqUnsorted ::
  P.Comparer -> FwdSeqUnsorted -> PostingInfo -> Bool
testFwdSeqUnsorted c f = P.comp c f fwdSeqUnsorted

testBackSeqUnsorted ::
  P.Comparer -> BackSeqUnsorted -> PostingInfo -> Bool
testBackSeqUnsorted c f = P.comp c f backSeqUnsorted

testBackSeqSorted ::
  P.Comparer -> BackSeqSorted -> PostingInfo -> Bool
testBackSeqSorted c f = P.comp c f backSeqSorted

