module Penny.Zinc.Parser where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success))
import Data.List (sortBy)
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed)

import qualified Penny.Zinc.Expressions as X
import qualified Penny.Zinc.Parser.Matchers as PM
import qualified Penny.Zinc.Parser.Operands as O
import qualified Penny.Zinc.Parser.Operators as Oo
import qualified Penny.Zinc.Parser.Sorter as S
import Penny.Zinc.Parser.Error (Error)

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

wrapOperand ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
wrapOperand dtz dt rad sep st =
  mkSt <$> O.parseToken dtz dt rad sep (factory st) where
    mkSt op = st { tokens = tokens st ++ [op'] } where
        op' = X.TokOperand (f . postingBox)
        (X.Operand f) = op

wrapOperator :: State -> ParserE Error State
wrapOperator st = mkSt <$> Oo.parser where
  mkSt f  = st { tokens = tokens st ++ [f] }

wrapMatcher :: State -> ParserE Error State
wrapMatcher st = mkSt <$> PM.parser cOld mOld where
  (cOld, mOld) = (sensitive st, factory st)
  mkSt (c, m) = st { sensitive = c, factory = m }
  
wrapOrderer :: State -> ParserE Error State
wrapOrderer st = mkSt <$> S.sort where
  mkSt or = st { orderer = or `mappend` (orderer st) }

parseOption ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseOption dtz dt rad sp st =
  wrapOperand dtz dt rad sp st
  <|> wrapOperator st
  <|> wrapMatcher st
  <|> wrapOrderer st


data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingInfo -> Bool)]
        , postFilter :: [PostingInfo] -> [PostingInfo]
        , orderer :: S.Orderer }


data PostingInfo =
  PostingInfo { postingBox :: PostingBox
              , fwdSeqUnsorted :: FwdSeqUnsorted
              , backSeqUnsorted :: BackSeqUnsorted
              , fwdSeqSorted :: FwdSeqSorted
              , backSeqSorted :: BackSeqSorted }

newtype FwdSeqSorted = FwdSeqSorted { unFwdSeqSorted :: Integer }
                       deriving Show

newtype BackSeqSorted = BackSeqSorted { unBackSeqSorted :: Integer }
                      deriving Show

newtype FwdSeqUnsorted = FwdSeqUnsorted { unFwdSeqUnsorted :: Integer }
                       deriving Show

newtype BackSeqUnsorted = BackSeqUnsorted { unBackSeqUnsorted :: Integer }
                        deriving Show

numberUnsorted ::
  [PostingBox]
  -> [(PostingBox, FwdSeqUnsorted, BackSeqUnsorted)]
numberUnsorted ps = reverse backSeqs where
  fwdSeqs = zipWith (,) ps (map FwdSeqUnsorted [0..])
  backSeqs = zipWith f (reverse fwdSeqs) bs where
    bs = (map BackSeqUnsorted [0..])
    f (pb, fu) bu = (pb, fu, bu)

sortTriples ::
  S.Orderer
  -> [(PostingBox, FwdSeqUnsorted, BackSeqUnsorted)]
  -> [(PostingBox, FwdSeqUnsorted, BackSeqUnsorted)]
sortTriples o ps = sortBy f ps where
  f (pb1, _, _) (pb2, _, _) = o pb1 pb2

numberSorted ::
  [(PostingBox, FwdSeqUnsorted, BackSeqUnsorted)]
  -> [PostingInfo]
numberSorted ps = reverse backNums where
  fwdNums = zipWith f ps (map FwdSeqSorted [0..]) where
    f (pb, fu, bu) fs = (pb, fu, bu, fs)
  backNums = zipWith f (reverse fwdNums) bss where
    f (pb, fu, bu, fs) bs = PostingInfo pb fu bu fs bs
    bss = map BackSeqSorted [0..]

