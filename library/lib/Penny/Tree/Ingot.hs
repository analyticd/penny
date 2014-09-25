-- | An Ingot is a Lewis paired with an optional Currency.

module Penny.Tree.Ingot where

import qualified Penny.Tree.Ingot.Period as Period
import qualified Penny.Tree.Ingot.Comma as Comma
import Text.Parsec.Text
import Control.Applicative

data T
  = Comma Comma.T
  | Period Period.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Comma <$> Comma.parser
  <|> Period <$> Period.parser
