module Penny.Zinc.Parser where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success))
import Data.List (sortBy)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed)

import qualified Penny.Zinc.Expressions as X
import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Sorter as S
import Penny.Zinc.Parser.Error (Error)

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

parseOption ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> (F.State, S.Orderer)
  -> ParserE Error (F.State, S.Orderer)
parseOption dtz dt rad sep (st, ord) =
  (\s -> (s, ord))    <$> F.parseToken dtz dt rad sep st
  <|> (\o -> (st, o)) <$> S.sort ord

parseOptions ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> (F.State, S.Orderer)
  -> ParserE Error (F.State, S.Orderer)
parseOptions dtz dt rad sep = feed (parseOption dtz dt rad sep)



data State =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (PostingInfo -> Bool)]
        , postFilter :: [PostingInfo] -> [PostingInfo] }


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

defaultState :: (F.State, S.Orderer)
defaultState = (F.blankState, mempty)
