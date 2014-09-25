module Penny.Tree.Side where

import qualified Penny.Tree.Than.Less as Less
import qualified Penny.Tree.Than.Greater as Greater
import Text.Parsec.Text
import Control.Applicative

data T
  = Debit Less.T
  | Credit Greater.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Debit <$> Less.parser
  <|> Credit <$> Greater.parser
