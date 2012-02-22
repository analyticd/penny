module Penny.Zinc.Parser where

import Control.Applicative ((<|>), (<$>))
import Data.Monoid (mempty)
import System.Console.MultiArg.Prim (ParserE, feed)

import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Sorter as S
import Penny.Zinc.Parser.Error (Error)

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)

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

defaultState :: (F.State, S.Orderer)
defaultState = (F.blankState, mempty)
