module Penny.Zinc.Parser where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success))
import Data.List (sortBy, intersperse, groupBy)
import Data.Monoid (mempty, mappend)
import qualified Data.Queue as Q
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed, throw)

import qualified Penny.Zinc.Expressions as X
import qualified Penny.Zinc.Parser.Matchers as PM
import qualified Penny.Zinc.Parser.Operands as O
import qualified Penny.Zinc.Parser.Operators as Oo
import qualified Penny.Zinc.Parser.PostFilters as PF
import qualified Penny.Zinc.Parser.Seq as PSq
import qualified Penny.Zinc.Parser.Sorter as S
import qualified Penny.Zinc.Parser.Types as T
import Penny.Zinc.Parser.Error (Error)
import qualified Penny.Zinc.Parser.Error as E

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (T.PostingInfo -> Bool)]
        , postFilter :: [T.PostingInfo] -> [T.PostingInfo]
        , orderer :: S.Orderer }

newState :: State
newState =
  State { sensitive = M.Insensitive
        , factory = \t -> return (M.within M.Insensitive t)
        , tokens = []
        , postFilter = id
        , orderer = mempty }

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
        op' = X.TokOperand (f . T.postingBox)
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

wrapSeq :: State -> ParserE Error State
wrapSeq st = mkSt <$> PSq.parser where
  mkSt op = st { tokens = tokens st ++ [op] }

wrapPostFilter :: State -> ParserE Error State
wrapPostFilter st = mkSt <$> PF.parser where
  mkSt fn = st { postFilter = fn . postFilter st }

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
  <|> wrapSeq st
  <|> wrapPostFilter st

parseOptions ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseOptions dtz dt rad sp = feed (parseOption dtz dt rad sp)

parsePostingInfos ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> ParserE Error ([PostingBox] -> [T.PostingInfo])
parsePostingInfos dtz dt rad sep = do
  let st = newState
  st' <- parseOptions dtz dt rad sep st
  p <- case getPredicate (tokens st') of
    Just pr -> return pr
    Nothing -> throw E.BadExpression
  let f pbs = filter p preFilt where
        preFilt = PSq.postingInfos (orderer st') pbs
  return f

-- | Takes the list of tokens and gets the predicate to use.
getPredicate :: 
  [X.Token (T.PostingInfo -> Bool)]
  -> Maybe (T.PostingInfo -> Bool)
getPredicate ls = X.evaluate q where
  q = foldl (flip Q.enqueue) Q.empty (insertAddTokens ls)

-- | If there is no operand between tokens, the And operand is
-- assumed; this function inserts that operand.
insertAddTokens :: [X.Token (a -> Bool)]
                   -> [X.Token (a -> Bool)]
insertAddTokens ts = concatMap inserter grouped where
  inserter = intersperse Oo.tokAnd
  grouped = groupBy f ts
  f x y = case (x, y) of
    (X.TokOperand _, X.TokOperand _) -> True
    _ -> False

